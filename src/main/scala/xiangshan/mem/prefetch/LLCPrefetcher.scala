package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu._
import xiangshan.mem.Bundles.LsPrefetchTrainBundle

trait HasLLCPrefetcherParam {
  val featureBits = 10
}

/*
*   Prediction Req to LLCPrefetcher
*   (ld inst -> trigger a prediction?)
*   we only use vaddr & uop
* */
class LLCPredReq()(implicit p: Parameters) extends XSBundle {
  val uop = new DynInst
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val pc    = UInt(VAddrBits.W)

  def fromMemExuInput(input: MemExuInput) = {
    this := DontCare
    this.uop    := input.uop
    this.vaddr  := input.src(0) + SignExt(input.uop.imm(11, 0), VAddrBits)
    this.pc     := input.uop.pc
  }

  def asLLCPrefetchReqBundle(): LLCPrefetchReqBundle = {
    val output = Wire(new LLCPrefetchReqBundle)
    output := DontCare
    output.vaddr := this.vaddr
    output.paddr := this.paddr
    output.pc := this.pc
    output.uop   := this.uop
    output
  }
}

trait HasLLCPrefetchHelper extends HasL1PrefetchHelper

trait HasLLCFilterHelper extends HasCircularQueuePtrHelper {
  def reorder[T <: MemExuInput](source: Vec[ValidIO[T]]): Vec[ValidIO[T]] = {
    require(source.length == 3, "only support 3 input")
    val res = Wire(source.cloneType)

    val source_v = source.map(_.valid)
    val robidx0 = source(0).bits.uop.robIdx
    val robidx1 = source(1).bits.uop.robIdx
    val robidx2 = source(2).bits.uop.robIdx

    // cmp
    val cmp01 = Mux1H(Seq(
      (Cat(source_v(0), source_v(1)).asUInt === "b00".U) -> false.B,
      (Cat(source_v(0), source_v(1)).asUInt === "b01".U) -> false.B,
      (Cat(source_v(0), source_v(1)).asUInt === "b10".U) -> true.B,
      (Cat(source_v(0), source_v(1)).asUInt === "b11".U) -> isBefore(robidx0, robidx1),
    ))

    val cmp02 = Mux1H(Seq(
      (Cat(source_v(0), source_v(2)).asUInt === "b00".U) -> false.B,
      (Cat(source_v(0), source_v(2)).asUInt === "b01".U) -> false.B,
      (Cat(source_v(0), source_v(2)).asUInt === "b10".U) -> true.B,
      (Cat(source_v(0), source_v(2)).asUInt === "b11".U) -> isBefore(robidx0, robidx2),
    ))

    val cmp12 = Mux1H(Seq(
      (Cat(source_v(1), source_v(2)).asUInt === "b00".U) -> false.B,
      (Cat(source_v(1), source_v(2)).asUInt === "b01".U) -> false.B,
      (Cat(source_v(1), source_v(2)).asUInt === "b10".U) -> true.B,
      (Cat(source_v(1), source_v(2)).asUInt === "b11".U) -> isBefore(robidx1, robidx2),
    ))

    // sort
    val sorted0 = Mux(cmp01, Mux(cmp02, source(0), source(2)), Mux(cmp12, source(1), source(2)))
    val sorted1 = Mux(cmp01, Mux(cmp12, source(1), Mux(cmp02, source(2), source(0))), Mux(cmp02, source(0), Mux(cmp12, source(2), source(1))))
    val sorted2 = Mux(cmp01, Mux(cmp12, source(2), source(1)), Mux(cmp02, source(2), source(0)))

    // res
    res(0) := sorted0
    res(1) := sorted1
    res(2) := sorted2

    res
  }
}

class FeatureBundle()(implicit p: Parameters) extends XSBundle with HasLLCPrefetcherParam {
  val pc_cl_offset_idx      = UInt(featureBits.W)
  val pc_cl_byte_offset_idx = UInt(featureBits.W)
  val pc_first_access_idx   = UInt(featureBits.W)
  val cl_first_access_idx   = UInt(featureBits.W)
  val last_pcs_idx          = UInt(featureBits.W)

  def connect_from_vec(input: Vec[UInt]) = {
    require(input.length == 5, "length must be 5")
    this.pc_cl_offset_idx      := input(0)
    this.pc_cl_byte_offset_idx := input(1)
    this.pc_first_access_idx   := input(2)
    this.cl_first_access_idx   := input(3)
    this.last_pcs_idx          := input(4)
  }
}

/**
 * Page Buffer: 64 recent pages (virtual or physical)
 * | Tag | Page Idx (6 bits) | Bitmap Idx (6 bits) | Cacheline Offset 6 (bits) |
 * */
class PageBuffer(AddrBits:Int, TrainPortNum:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val req_addr = Input(UInt(AddrBits.W))
    val train_addr = Flipped(Vec(TrainPortNum, ValidIO(UInt(AddrBits.W))))
    val is_accessed = Output(Bool())
  })

  val TagBits = AddrBits - 6 - 12

  val tags = RegInit(VecInit(Seq.fill(64){0.U(TagBits.W)}))
  val valids = RegInit(VecInit(Seq.fill(64){false.B}))
  val bitmaps = RegInit(VecInit(Seq.fill(64){0.U(64.W)}))

  def parse_addr(addr: UInt) = {
    val bitmap_idx = addr(11, 5)
    val idx = addr(17, 12)
    val tag = addr(AddrBits - 1, 18)
    (bitmap_idx, idx, tag)
  }

  // Proccess Req
  val (req_bm_idx, req_idx, req_tag) = parse_addr(io.req_addr)
  val req_hit = tags(req_idx) === req_tag && valids(req_idx)
  io.is_accessed  := req_hit && bitmaps(req_idx)(req_bm_idx)

  // Process Train
  // TODO: hit same entry?
  for (i <- 0 until TrainPortNum) {
    val (train_bm_idx, train_idx, train_tag) = parse_addr(io.train_addr(i).bits)
    val train_hit = tags(train_idx) === train_tag && valids(train_idx)
    when (io.train_addr(i).valid){
      valids(train_idx)   := true.B
      tags(train_idx)     := Mux(train_hit, tags(train_idx), train_tag)
      bitmaps(train_idx)  := Mux(train_hit, bitmaps(train_idx) | (1.U << train_bm_idx).asUInt, 1.U << train_bm_idx)
    }
  }

}

/*
*   Prefetch Train Req to LLCPrefetcher
*   (commit ld inst -> trigger a train?)
* */
class LLCTrainReq()(implicit p: Parameters) extends XSBundle {
  val need_dec = Bool()
  val need_inc = Bool()
  val vaddr = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)
  val drop = Bool()
  // TODO: add features here
  val feat_idx = new FeatureBundle()
}

class LLCRecordBundle()(implicit p: Parameters) extends XSBundle{
  val uop = new DynInst
  val vaddr = UInt(VAddrBits.W)
  val above_threshold = Bool()
  // TODO: add features here and recored them in the train filter
  val feat_idx = new FeatureBundle()

  val issue_pft = Bool()
}

class LLCTrainFilter(size: Int, name: String)(implicit p: Parameters) extends XSModule with HasTrainFilterHelper with HasLLCPrefetchHelper {
  val io = IO(new Bundle{
    val ld_res = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LsPrefetchTrainBundle))) // max 3/cycle
    val pft_rec = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LLCRecordBundle()))) // max 3/cycle
    val train_req = Vec(backendParams.LdExuCnt, ValidIO(new LLCTrainReq()))

    val upt_vaddr = Vec(backendParams.LdExuCnt, ValidIO(UInt(VAddrBits.W)))   // for updating access table
    val upt_pc    = Vec(backendParams.LdExuCnt, ValidIO(UInt(VAddrBits.W)))   // for updating last pcs
  })
  val (res, rec) = (io.ld_res, io.pft_rec)

  // for accuracy perfing
  val correct_seq = Seq.fill(backendParams.LdExuCnt){RegInit(0.U(XLEN.W))}
  val incorrect_seq = Seq.fill(backendParams.LdExuCnt){RegInit(0.U(XLEN.W))}

  //val correct_offchip_seq = Seq.fill(backendParams.LdExuCnt){RegInit(0.U(XLEN.W))}
  //val incorret_offchip_seq = Seq.fill(backendParams.LdExuCnt){RegInit(0.U(XLEN.W)

  // train
  val train_req = io.train_req
  for (i <- 0 until backendParams.LdExuCnt) {
    val randval = LFSR64(seed=Some(19260817 + i))
    val rateMask = Constantin.createRecord("DropRateMask", 1)
    val drop = !res(i).bits.is_offchip && Mux(rateMask === 0.U, false.B, (randval & rateMask) === 0.U)

    train_req(i).valid  := res(i).valid && rec(i).valid
    train_req(i).bits.pc    := res(i).bits.uop.pc
    train_req(i).bits.vaddr := res(i).bits.vaddr
    train_req(i).bits.drop  := drop
    train_req(i).bits.need_dec  := !res(i).bits.is_offchip && rec(i).bits.issue_pft && rec(i).bits.issue_pft === rec(i).bits.above_threshold
    train_req(i).bits.need_inc  := res(i).bits.is_offchip && !rec(i).bits.issue_pft && rec(i).bits.issue_pft === rec(i).bits.above_threshold
    train_req(i).bits.feat_idx  := rec(i).bits.feat_idx

    XSPerfAccumulate(s"OffchipLoadTrain$i",  train_req(i).valid && train_req(i).bits.need_inc)
    XSPerfAccumulate(s"NonOffchipLoadTrain$i", train_req(i).valid && train_req(i).bits.need_dec)
    XSPerfAccumulate(s"OffchipLoadTrainDrop$i", train_req(i).valid && train_req(i).bits.need_inc && drop)
    XSPerfAccumulate(s"NonOffchipLoadTrainDrop$i", train_req(i).valid && train_req(i).bits.need_dec && drop)
  }

  for (i <- 0 until backendParams.LdExuCnt) {
    val v = res(i).valid && rec(i).valid

    io.upt_pc(i).valid  := v
    io.upt_pc(i).bits   := res(i).bits.uop.pc

    io.upt_vaddr(i).valid := v
    io.upt_vaddr(i).bits  := res(i).bits.vaddr
  }

  // PerfCnt
  for (i <- 0 until backendParams.LdExuCnt){
    val v = res(i).valid && rec(i).valid

    XSPerfAccumulate(s"correctLLCPrefetch$i", v && res(i).bits.is_offchip === rec(i).bits.issue_pft)
    XSPerfAccumulate(s"incorrectLLCPrefetch$i", v && res(i).bits.is_offchip =/= rec(i).bits.issue_pft)
    XSPerfAccumulate(s"correctOffchipLLCPrefetch$i", v && res(i).bits.is_offchip === rec(i).bits.issue_pft && res(i).bits.is_offchip)
    XSPerfAccumulate(s"incorrectOffchipLLCPrefetch$i", v && res(i).bits.is_offchip =/= rec(i).bits.issue_pft && res(i).bits.is_offchip)
  }
}

/*
*   Prediction Req Filter
*   ldu <-> PredReqFilter <-> PrefetcherCore
* */
class LLCPredReqFilter(size: Int, name: String)(implicit p: Parameters) extends XSModule with HasLLCFilterHelper with HasLLCPrefetchHelper {
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect))
    val enable = Input(Bool())
    val flush = Input(Bool())
    val ld_in = Flipped(Vec(backendParams.LduCnt, ValidIO(new MemExuInput())))
    val pred_req = DecoupledIO(new LLCPredReq())
  })

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => size ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = Reg(Vec(size, new LLCPredReq()))
  val valids = RegInit(VecInit(Seq.fill(size){ false.B }))
  val is_flushed = RegInit(VecInit(Seq.fill(size){ false.B}))

  // enq
  val enqLen = backendParams.LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))

  val deqPtr = WireInit(deqPtrExt.value)

  require(size >= enqLen)

  val ld_in_reordered = reorder(io.ld_in)
  val reqs_l = Wire(Vec(enqLen, new LLCPredReq()))
  for (i <- 0 until enqLen){
    reqs_l(i).fromMemExuInput(ld_in_reordered(i).bits)
  }

  val reqs_vl = ld_in_reordered.map(_.valid)
  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))

  for(i <- 0 until enqLen) {
    val req = reqs_l(i)
    val req_v = reqs_vl(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)

    val entry_match = Cat(entries.zip(valids).map {
      case (e, v) => v && block_addr(e.vaddr) === block_addr(req.vaddr)
    }).orR

    val prev_enq_match = if(i == 0) false.B else Cat(reqs_l.zip(reqs_vl).take(i).map {
      case(pre, pre_v) => pre_v && block_addr(pre.vaddr) === block_addr(req.vaddr)
    }).orR

    needAlloc(i) := req_v && !entry_match && !prev_enq_match && !req.uop.robIdx.needFlush(io.redirect)
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt && io.enable

    XSPerfAccumulate(s"failToEnqPredReqFilter$i", needAlloc(i) && allocPtr < deqPtrExt)

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
      is_flushed(allocPtr.value)  := false.B
    }
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach(x => when(canAlloc.asUInt.orR) {x := x + allocNum})

  for (i <- 0 until size) {
    when (valids(i) && entries(i).uop.robIdx.needFlush(io.redirect)){
      is_flushed(i) := false.B
    }
  }

  // deq
  io.pred_req.valid := false.B
  io.pred_req.bits := DontCare
  valids.zip(entries).zipWithIndex.foreach {
    case((valid, entry), i) =>
      when(deqPtr === i.U) {
        io.pred_req.valid := valid && io.enable && !is_flushed(i) && !entry.uop.robIdx.needFlush(io.redirect)
        io.pred_req.bits := entry
      }
  }

  when(io.pred_req.fire) {
    valids(deqPtr) := false.B
    deqPtrExt := deqPtrExt + 1.U
  }

  when(valids(deqPtr) && is_flushed(deqPtr)){
    deqPtrExt := deqPtrExt + 1.U
  }

  when(RegNext(io.flush)) {
    valids.foreach(valid => valid := false.B)
    (0 until enqLen).map(i => enqPtrExt(i) := i.U.asTypeOf(new Ptr))
    deqPtrExt := 0.U.asTypeOf(new Ptr)
  }
}

/*
*   Prefetch Filter
*   PrefetcherCore <-> pftFilter <-> L2 Cache
* */

class LLCPrefetchFilterEntry()(implicit p: Parameters) extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val paddr_valid = Bool()
  val uop = new DynInst
}

class LLCPrefetchReqBundle()(implicit p: Parameters) extends PrefetchReqBundle{
  val uop = new DynInst
}

class LLCPrefetchFilter(size: Int, name: String)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle(){
    val gen_req = Flipped(ValidIO(new LLCPrefetchReqBundle()))
    val tlb_req = new TlbRequestIO(2)
    val pmp_resp = Flipped(new PMPRespBundle())

    val llc_pf_req = ValidIO(new Bundle{
      val paddr = UInt(PAddrBits.W)
      val uop   = new DynInst
    })
  })

  val entries = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(new LLCPrefetchFilterEntry()))))
  val valids = RegInit(VecInit(Seq.fill(size)(false.B)))

  val l3_pf_req_arb = Module(new RRArbiterInit(new Bundle{
    val paddr = UInt(PAddrBits.W)
    val uop = new DynInst
  }, size))

  val tlb_req_arb = Module(new RRArbiterInit(new TlbReq, size))

  // enq logic
  // TODO: add vaddr & paddr hit check
  // TODO: add plru if there is no available entry
  val availableVec = VecInit(valids.map(e => !e))
  val canAlloc = availableVec.asUInt.orR
  val allocIdx = OHToUInt(availableVec)
  assert(!io.gen_req.valid || canAlloc, "currently we always have free entry.")

  val alloc_entry = entries(allocIdx)
  when (io.gen_req.valid){
    alloc_entry.vaddr := io.gen_req.bits.vaddr
    alloc_entry.uop   := io.gen_req.bits.uop
    alloc_entry.paddr_valid := false.B

    valids(allocIdx) := true.B
  }

  // deq logic
  for (i <- 0 until size) {
    l3_pf_req_arb.io.in(i).valid  := entries(i).paddr_valid && valids(i)
    l3_pf_req_arb.io.in(i).bits.paddr := entries(i).paddr
    l3_pf_req_arb.io.in(i).bits.uop   := entries(i).uop

    when (l3_pf_req_arb.io.in(i).fire){
      valids(i) := false.B
    }
  }
  l3_pf_req_arb.io.out.ready  := true.B
  io.llc_pf_req.valid         := l3_pf_req_arb.io.out.valid
  io.llc_pf_req.bits          := l3_pf_req_arb.io.out.bits

  // query tlb & pmp
  io.tlb_req.resp.ready := true.B
  io.tlb_req.req_kill := false.B
  io.tlb_req.req  <> tlb_req_arb.io.out
  val tlb_resp = io.tlb_req.resp
  val pmp_resp = io.pmp_resp
  for (i <- 0 until size) {
    val has_tlb_req_fire = RegNext(tlb_req_arb.io.in(i).fire, false.B)

    tlb_req_arb.io.in(i).valid := valids(i) && !entries(i).paddr_valid && !has_tlb_req_fire
    tlb_req_arb.io.in(i).bits := DontCare
    tlb_req_arb.io.in(i).bits.vaddr := entries(i).vaddr
    tlb_req_arb.io.in(i).bits.cmd := TlbCmd.read
    tlb_req_arb.io.in(i).bits.isPrefetch  := true.B
    tlb_req_arb.io.in(i).bits.size := 3.U
    tlb_req_arb.io.in(i).bits.kill := false.B
    tlb_req_arb.io.in(i).bits.no_translate := false.B
    tlb_req_arb.io.in(i).bits.fullva := 0.U
    tlb_req_arb.io.in(i).bits.checkfullva := false.B

    /*
    *   Cycle 1: tlb req fire
    *   Cycle 2: tlb & pmp resp arrives
    * */
    val update_valid = has_tlb_req_fire && tlb_resp.fire && !tlb_resp.bits.miss
    val drop = tlb_resp.bits.excp.head.pf.ld || tlb_resp.bits.excp.head.gpf.ld || tlb_resp.bits.excp.head.af.ld ||  // page/access fault
      pmp_resp.mmio || Pbmt.isUncache(tlb_resp.bits.pbmt.head) || // uncache
      pmp_resp.ld   // pmp access fault
    when (update_valid){
      valids(i) := Mux(drop, false.B, valids(i))

      entries(i).paddr  := Cat(tlb_resp.bits.paddr.head(PAddrBits - 1, log2Ceil(64)), 0.U(log2Ceil(64).W))  // TODO: parameterize
      entries(i).paddr_valid  := !drop
    }
  }
}

class BaseLLCPrefetcher()(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle(){
    val redirect = Flipped(Valid(new Redirect))
    val pred_req = Flipped(DecoupledIO(new LLCPredReq()))
    val pft_req = ValidIO(new LLCPrefetchReqBundle())
    val pft_rec = ValidIO(new LLCRecordBundle())
    val train_req = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LLCTrainReq())))
  })
}

class RandomLLCPrefetcher()(implicit p: Parameters) extends BaseLLCPrefetcher {
  io.pred_req.ready := true.B

  val rateMask = Constantin.createRecord("RandomLLCPftRateMask", 1)
  val randval = LFSR64()
  io.pft_req.valid  := RegNext(io.pred_req.fire && (randval & rateMask) === 0.U)
  io.pft_req.bits   := RegNext(io.pred_req.bits.asLLCPrefetchReqBundle())

  io.pft_rec.valid  := RegNext(io.pred_req.fire)
  io.pft_rec.bits   := DontCare
  io.pft_rec.bits.uop := RegNext(io.pred_req.bits.uop)
  io.pft_rec.bits.above_threshold := RegNext(io.pred_req.fire && (randval & rateMask) === 0.U)
  io.pft_rec.bits.vaddr := RegNext(io.pred_req.bits.vaddr)
}

class PerceptronLLCPrefetcher(last_pc_num: Int = 4)(implicit p: Parameters) extends BaseLLCPrefetcher with HasLLCPrefetcherParam {
  val io_upt_vaddr  = IO(Flipped(Vec(backendParams.LdExuCnt, ValidIO(UInt(VAddrBits.W)))))
  val io_upt_pc     = IO(Flipped(Vec(backendParams.LdExuCnt, ValidIO(UInt(VAddrBits.W)))))

  io.pred_req.ready := true.B

  def get_cl_offset(vaddr: UInt) = {
    vaddr(11, 6)   // TODO: Parameterized
  }

  def get_cl_byte_offset(vaddr: UInt) = {
    vaddr(5, 0)    // TODO: Parameterized
  }

  def hash_simple(value: UInt) = {
    ZeroExt(value, 32)(featureBits - 1, 0)
  }

  def hash_fold(value: UInt) = {
    val ext_val = ZeroExt(value, 4 * featureBits)
    val slices_val = Wire(Vec(4, UInt(featureBits.W)))
    for (i <- 0 until 4){
      slices_val(i) := ext_val((i + 1) * featureBits - 1, i * featureBits)
    }
    slices_val.reduce(_ ^ _)
  }

  // Access Table
  val pbuffer = Module(new PageBuffer(AddrBits = VAddrBits, TrainPortNum = backendParams.LdExuCnt))
  pbuffer.io.req_addr   := io.pred_req.bits.vaddr
  pbuffer.io.train_addr <> io_upt_vaddr

  // Last Load PC
  val last_pcs = RegInit(VecInit(Seq.fill(last_pc_num){0.U(VAddrBits.W)}))
  val pcs = io_upt_pc.map(_.bits)
  val pcv = io_upt_pc.map(_.valid)

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => last_pc_num ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val enqLen = backendParams.LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val needAlloc = Wire(Vec(enqLen, Bool()))

  for (i <- 0 until enqLen) {
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)

    needAlloc(i)  := pcv(i)
    when (needAlloc(i)){
      last_pcs(allocPtr.value)  := pcs(i)
    }
  }

  val allocNum = PopCount(needAlloc)
  enqPtrExt.foreach(x => when(needAlloc.asUInt.orR) {x := x + allocNum})

  // Weight
  val init_val = Constantin.createRecord("weightInitVal", -4)
  val w_matrix = Reg(Vec(5, Vec(1 << featureBits, SInt(8.W))))
  when (reset.asBool) {
    for (i <- 0 until 5){
      for (j <- 0 until (1 << featureBits)){
        w_matrix(i)(j) := init_val.asSInt    // TODO: use pre-trained weight?
      }
    }
  }

  // Weight Training
  val TN = Constantin.createRecord("LLCPredictorWLowerBound", -35).asSInt
  val TP = Constantin.createRecord("LLCPredictorWUpperBound", 40).asSInt

  def update_dec(old_value: SInt, TN: SInt, delta: SInt = 1.S) = {
    Mux(old_value - delta >= TN, old_value - delta, TN)
  }

  def update_inc(old_value: SInt, TP: SInt, delta: SInt = 1.S) = {
    Mux(old_value + delta <= TP, old_value + delta, TP)
  }

  for (i <- 0 until 5){
    val v = Wire(Vec(backendParams.LdExuCnt, Bool()))
    val idx = Wire(Vec(backendParams.LdExuCnt, UInt(featureBits.W)))
    val inc = Wire(Vec(backendParams.LdExuCnt, Bool()))
    val dec = Wire(Vec(backendParams.LdExuCnt, Bool()))

    for (j <- 0 until backendParams.LdExuCnt){
      val train_feat_idx = io.train_req(j).bits.feat_idx.elements.toSeq.map(_._2)

      v(j)    := io.train_req(j).valid && !io.train_req(j).bits.drop
      idx(j)  := train_feat_idx(i)

      inc(j)  := io.train_req(j).bits.need_inc
      dec(j)  := io.train_req(j).bits.need_dec

      assert(inc(j) ^ dec(j) || !dec(j) && !inc(j))
    }

    for (j <- 0 until (1 << featureBits)){
      val need_inc = v.zip(idx).zip(inc).map{
        case ((v, idx), inc)  =>
          v && idx === j.U && inc
      }
      val need_dec = v.zip(idx).zip(dec).map{
        case ((v, idx), dec)  =>
          v && idx === j.U && dec
      }

      val inc_cnt = PopCount(need_inc)
      val dec_cnt = PopCount(need_dec)

      when (inc_cnt > dec_cnt) {
        w_matrix(i)(j)  := update_inc(w_matrix(i)(j), TP.asSInt, (inc_cnt - dec_cnt).asSInt)
      }

      when (dec_cnt > inc_cnt) {
        w_matrix(i)(j)  := update_dec(w_matrix(i)(j), TN.asSInt, (dec_cnt - inc_cnt).asSInt)
      }
    }
  }

  /**
   *  Saturation Counter
   *  Represent recent prediction's accuracy
   *  Range: 0~9
   * */
  val sat_cnt = RegInit(0.S(5.W))
  val correct_v = io.train_req.map{
    case e =>
      e.valid && !(e.bits.need_inc || e.bits.need_dec)
  }
  val incorrect_v = io.train_req.map{
    case e =>
      e.valid && (e.bits.need_inc || e.bits.need_dec)
  }

  val correct_cnt = PopCount(correct_v)
  val incorrect_cnt = PopCount(incorrect_v)
  val sat_inc = correct_cnt > incorrect_cnt
  val sat_dec = correct_cnt < incorrect_cnt
  val inc_delta = correct_cnt - incorrect_cnt
  val dec_delta = incorrect_cnt - correct_cnt

  when (sat_inc){
    sat_cnt := Mux((sat_cnt + inc_delta.asSInt) >= 9.S, 9.S, sat_cnt + inc_delta.asSInt)
  }

  when (sat_dec){
    sat_cnt := Mux((sat_cnt - dec_delta.asSInt) <= 0.S, 0.S, sat_cnt - dec_delta.asSInt)
  }

  val use_sat_cnt = Constantin.createRecord("useSaturateCnt", false)
  val sat_positive = use_sat_cnt && (sat_cnt >= 5.S) || !use_sat_cnt

  /*------------------------------ s1 ------------------------------*/
  val s1_valid = io.pred_req.valid && !io.pred_req.bits.uop.robIdx.needFlush(io.redirect)

  // Calculate Features
  val vaddr = io.pred_req.bits.vaddr
  val pc = io.pred_req.bits.pc

  val cl_offset = get_cl_offset(vaddr)
  val cl_byte_offset = get_cl_byte_offset(vaddr)
  val first_access = !pbuffer.io.is_accessed

  val pc_cl_offset  = pc ^ cl_offset
  val pc_cl_offset_idx = hash_fold(pc_cl_offset)

  val pc_cl_byte_offset = pc ^ cl_byte_offset
  val pc_cl_byte_offset_idx = hash_fold(pc_cl_byte_offset)

  val pc_first_access = Cat(pc, first_access)
  val pc_first_access_idx = hash_fold(pc_first_access)

  val cl_first_access = Cat(cl_offset, first_access)
  val cl_first_access_idx = hash_fold(cl_first_access)

  val last_pc = last_pcs.reduce(_ ^ _)
  val last_pc_idx = hash_fold(last_pc)

  // Get Weight
  val feat_idx = VecInit(pc_cl_offset_idx, pc_cl_byte_offset_idx, pc_first_access_idx, cl_first_access_idx, last_pc_idx)
  val weights = RegInit(VecInit(Seq.fill(5){0.S(8.W)}))

  for (i <- 0 until 5){
    weights(i)  := w_matrix(i)(feat_idx(i))
  }

  /*------------------------------ s2 ------------------------------*/
  val req_reg = RegNext(io.pred_req.bits)
  val feat_idx_reg = RegNext(feat_idx)

  val s2_valid  = RegNext(s1_valid) && !req_reg.uop.robIdx.needFlush(io.redirect)

  val tau_act = Constantin.createRecord("LLCPredictorActThreshold", -18)
  val sum = weights.reduce(_ + _)
  val above_threshold = sum > tau_act.asSInt
  dontTouch(sum)

  // to prefetch filter
  io.pft_req.valid  := above_threshold && s2_valid && sat_positive
  io.pft_req.bits   := DontCare
  io.pft_req.bits.pc    := req_reg.pc
  io.pft_req.bits.vaddr := req_reg.vaddr
  io.pft_req.bits.uop   := req_reg.uop

  // to VirtualLoadQueue
  io.pft_rec.valid  := s2_valid
  io.pft_rec.bits   := DontCare
  io.pft_rec.bits.vaddr := req_reg.vaddr
  io.pft_rec.bits.uop   := req_reg.uop
  io.pft_rec.bits.above_threshold := above_threshold
  io.pft_rec.bits.feat_idx.connect_from_vec(feat_idx_reg)

}

class LLCPrefetcher()(implicit p: Parameters) extends BasePrefecher {
  val use_perceptron = Constantin.createRecord("usePerceptronLLCPft", true)
  val use_random = Constantin.createRecord("useRandomLLCPft", false)

  assert(use_random ^ use_perceptron, "only one LLC pft could be in effect")

  val io_pred_in = IO(Flipped(Vec(backendParams.LdExuCnt, ValidIO(new MemExuInput))))
  val io_rec_req = IO(ValidIO(new LLCRecordBundle))
  val io_rec_upt_req = IO(ValidIO(new LLCRecordBundle))
  val io_rec_rsp = IO(Vec(backendParams.LdExuCnt, Flipped(ValidIO(new LLCRecordBundle))))
  val io_redirect = IO(Flipped(Valid(new Redirect)))
  val io_l1_pf_req   = IO(Flipped(ValidIO(UInt(PAddrBits.W))))
  val io_sms_pf_req  = IO(Flipped(ValidIO(UInt(PAddrBits.W))))

  val pred_filter = Module(new LLCPredReqFilter(size = 16, name = "pred_filter"))
  val pft_filter = Module(new LLCPrefetchFilter(size = 16, name = "pft_filter"))
  val train_filter = Module(new LLCTrainFilter(size = 72, name = "train_filter"))

  val perceptron_prefetcher = Module(new PerceptronLLCPrefetcher())
  val random_prefetcher = Module(new RandomLLCPrefetcher())

  // redirect
  val use_redirect = Constantin.createRecord("llcUseRedirect", true)

  perceptron_prefetcher.io.redirect.valid := io_redirect.valid & use_redirect
  perceptron_prefetcher.io.redirect.bits  := io_redirect.bits
  random_prefetcher.io.redirect.valid     := io_redirect.valid & use_redirect
  random_prefetcher.io.redirect.bits      := io_redirect.bits
  pred_filter.io.redirect.valid           := io_redirect.valid & use_redirect
  pred_filter.io.redirect.bits            := io_redirect.bits

  // default
  io_rec_req  <> DontCare
  pred_filter.io.pred_req.ready := false.B
  pft_filter.io.gen_req <> DontCare

  pred_filter.io.enable := io.enable
  pred_filter.io.flush  := false.B
  pred_filter.io.ld_in  <> io_pred_in

  when (use_perceptron){
    perceptron_prefetcher.io.pred_req   <> pred_filter.io.pred_req
    perceptron_prefetcher.io.pft_req    <> pft_filter.io.gen_req
    perceptron_prefetcher.io.pft_rec    <> io_rec_req
    perceptron_prefetcher.io.train_req  <> train_filter.io.train_req
  }.otherwise{
    perceptron_prefetcher.io.pred_req   <> DontCare
    perceptron_prefetcher.io.pft_req    <> DontCare
    perceptron_prefetcher.io.pft_rec    <> DontCare
    perceptron_prefetcher.io.train_req  <> DontCare
  }

  when (use_random){
    random_prefetcher.io.pred_req   <> pred_filter.io.pred_req
    random_prefetcher.io.pft_req    <> pft_filter.io.gen_req
    random_prefetcher.io.pft_rec    <> io_rec_req
    random_prefetcher.io.train_req  <> train_filter.io.train_req
  }.otherwise{
    random_prefetcher.io.pred_req   <> DontCare
    random_prefetcher.io.pft_req    <> DontCare
    random_prefetcher.io.pft_rec    <> DontCare
    random_prefetcher.io.train_req  <> DontCare
  }

  perceptron_prefetcher.io_upt_vaddr  <> train_filter.io.upt_vaddr
  perceptron_prefetcher.io_upt_pc     <> train_filter.io.upt_pc

  pft_filter.io.tlb_req   <> io.tlb_req
  pft_filter.io.pmp_resp  <> io.pmp_resp

  train_filter.io.ld_res  <> io.ld_in
  train_filter.io.pft_rec <> io_rec_rsp

  val llc_pf_req = pft_filter.io.llc_pf_req

  // magic?
  val pft_pbuffer = Module(new PageBuffer(AddrBits = PAddrBits, TrainPortNum = 2))
  pft_pbuffer.io.req_addr := llc_pf_req.bits.paddr

  pft_pbuffer.io.train_addr(0).valid  := io_l1_pf_req.valid
  pft_pbuffer.io.train_addr(0).bits   := io_l1_pf_req.bits

  pft_pbuffer.io.train_addr(1).valid  := io_sms_pf_req.valid
  pft_pbuffer.io.train_addr(1).bits   := io_sms_pf_req.bits

  val use_pft_acc_tab = Constantin.createRecord("usePftAccTab", false)
  val pft_acc_tab_allow = !pft_pbuffer.io.is_accessed && use_pft_acc_tab || !use_pft_acc_tab

  val is_valid_addr = PmemRanges.map(_.cover(llc_pf_req.bits.paddr)).reduce(_ || _)
  io.l3_req.valid := llc_pf_req.valid && is_valid_addr && pft_acc_tab_allow
  io.l3_req.bits  := llc_pf_req.bits.paddr

  io_rec_upt_req.valid  := llc_pf_req.valid
  io_rec_upt_req.bits   := DontCare
  io_rec_upt_req.bits.uop       := llc_pf_req.bits.uop
  io_rec_upt_req.bits.issue_pft := llc_pf_req.valid && is_valid_addr && pft_acc_tab_allow

  io.l1_req.valid := false.B
  io.l1_req.bits  := DontCare

  io.l2_req.valid := false.B
  io.l2_req.bits  := DontCare
}