package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.Bundles.{MemExuInput, DynInst}
import xiangshan.cache.mmu._
import xiangshan.mem.{LdPrefetchTrainBundle, LqWriteBundle}

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

  def asPrefetchReqBundle(): PrefetchReqBundle = {
    val output = Wire(new PrefetchReqBundle)
    output := DontCare
    output.vaddr := this.vaddr
    output.paddr := this.paddr
    output.pc := this.pc
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

/*
*   Prefetch Train Req to LLCPrefetcher
*   (commit ld inst -> trigger a train?)
* */
class LLCTrainReq()(implicit p: Parameters) extends XSBundle{
  val need_dec = Bool()
  val need_inc = Bool()
  // TODO: add features here
}

class LLCRecordBundle()(implicit p: Parameters) extends XSBundle{
  val uop = new DynInst
  val above_threshold = Bool()
  // TODO: add features here and recored them in the train filter
}

class LLCTrainFilter(size: Int, name: String)(implicit p: Parameters) extends XSModule with HasTrainFilterHelper with HasLLCPrefetchHelper {
  val io = IO(new Bundle{
    val ld_res = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LdPrefetchTrainBundle))) // max 3/cycle
    val pft_rec = Flipped(ValidIO(new LLCRecordBundle)) // 1/cycle
    val train_req = ValidIO(new LLCTrainReq())
  })

  val entries = Reg(Vec(size, new LLCRecordBundle))
  val valids = RegInit(VecInit(Seq.fill(size){false.B}))
  val commits = RegInit(VecInit(Seq.fill(size){false.B}))

  val train_req_arb = Module(new RRArbiterInit(new LLCTrainReq, size))

  // alloc
  val availableVec = VecInit(valids.map(e => !e))
  val canAlloc = availableVec.asUInt.orR
  val allocIdx = OHToUInt(availableVec)
  assert(!io.pft_rec.valid || canAlloc, "currently we always have free entry.")

  when (io.pft_rec.valid){
    entries(allocIdx) := io.pft_rec.bits
    valids(allocIdx)  := true.B
    commits(allocIdx) := false.B
  }

  // update
  for (i <- 0 until backendParams.LdExuCnt) {
    val update = VecInit(entries.zip(valids).map{ case (e, v) =>
      v && e.uop.robIdx === io.ld_res(i).bits.uop.robIdx
    })
    val update_idx = OHToUInt(update)
    val need_update = update.asUInt.orR
    when (io.ld_res(i).valid && need_update) {
      commits(update_idx) := true.B
      // TODO: res is correct?
    }
  }

  // deq logic
  for (i <- 0 until size){
    train_req_arb.io.in(i).valid  := valids(i) && commits(i)
    train_req_arb.io.in(i).bits   := DontCare
    train_req_arb.io.in(i).bits.need_dec  := false.B    // TODO
    train_req_arb.io.in(i).bits.need_inc  := false.B    // TODO

    when (train_req_arb.io.in(i).fire){
      valids(i)   := false.B
      commits(i)  := false.B
    }
  }
  train_req_arb.io.out.ready  := true.B
  io.train_req.valid  := train_req_arb.io.out.valid
  io.train_req.bits   := train_req_arb.io.out.bits

  XSPerfAccumulate("llc_train_req", io.train_req.valid)

  // for debug
  val entryTimer = RegInit(VecInit(Seq.fill(size)(0.U(16.W))))
  valids.zip(commits).zip(entryTimer).zipWithIndex.map{ case (((v, c), t), i) =>
    when (v && !c) { t := t + 1.U}
    when(RegNext(v && !c, false.B) && !(v && !c)) { t := 0.U }
    assert(t < 20000.U, "LLCPftTrainBuf Leak(id: %d)", i.U)
  }
}

/*
*   Prediction Req Filter
*   ldu <-> PredReqFilter <-> PrefetcherCore
* */
class LLCPredReqFilter(size: Int, name: String)(implicit p: Parameters) extends XSModule with HasLLCFilterHelper with HasLLCPrefetchHelper {
  val io = IO(new Bundle() {
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

    needAlloc(i) := req_v && !entry_match && !prev_enq_match
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt && io.enable

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
    }
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach(x => when(canAlloc.asUInt.orR) {x := x + allocNum})

  // deq
  io.pred_req.valid := false.B
  io.pred_req.bits := DontCare
  valids.zip(entries).zipWithIndex.foreach {
    case((valid, entry), i) =>
      when(deqPtr === i.U) {
        io.pred_req.valid := valid && io.enable
        io.pred_req.bits := entry
      }
  }

  when(io.pred_req.fire) {
    valids(deqPtr) := false.B
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
}

class LLCPrefetchFilter(size: Int, name: String)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle(){
    val gen_req = Flipped(ValidIO(new PrefetchReqBundle()))
    val tlb_req = new TlbRequestIO(2)
    val pmp_resp = Flipped(new PMPRespBundle())

    val llc_pf_addr = ValidIO(UInt(PAddrBits.W))
  })

  val entries = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(new LLCPrefetchFilterEntry()))))
  val valids = RegInit(VecInit(Seq.fill(size)(false.B)))

  val l3_pf_req_arb = Module(new RRArbiterInit(UInt(PAddrBits.W), size))
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
    alloc_entry.paddr_valid := false.B

    valids(allocIdx) := true.B
  }

  // deq logic
  for (i <- 0 until size) {
    l3_pf_req_arb.io.in(i).valid  := entries(i).paddr_valid && valids(i)
    l3_pf_req_arb.io.in(i).bits   := entries(i).paddr

    when (l3_pf_req_arb.io.in(i).fire){
      valids(i) := false.B
    }
  }
  l3_pf_req_arb.io.out.ready  := true.B
  io.llc_pf_addr.valid  := l3_pf_req_arb.io.out.valid
  io.llc_pf_addr.bits   := l3_pf_req_arb.io.out.bits

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

/*
*   Dummy Prefetcher
*   use random number to decide wheter issue a prefetch request.
* */
class BaseLLCPrefetcer()(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle(){
    val pred_req = Flipped(DecoupledIO(new LLCPredReq()))
    val pft_req = ValidIO(new PrefetchReqBundle())
    val pft_rec = ValidIO(new LLCRecordBundle())
    val train_req = Flipped(ValidIO(new LLCTrainReq()))
  })
}

class DummyLLCPrefetcher()(implicit p: Parameters) extends BaseLLCPrefetcer {
  io.pred_req.ready := true.B

  io.pft_req.valid  := RegNext(io.pred_req.fire && LFSR64()(0))
  io.pft_req.bits   := RegNext(io.pred_req.bits.asPrefetchReqBundle())

  io.pft_rec.valid  := RegNext(io.pred_req.fire)
  io.pft_rec.bits.uop := io.pred_req.bits.uop
  io.pft_rec.bits.above_threshold := RegNext(io.pred_req.fire && LFSR64()(0))
}

class LLCPrefetcher()(implicit p: Parameters) extends BasePrefecher {
  val io_pred_in = IO(Flipped(Vec(backendParams.LdExuCnt, ValidIO(new MemExuInput))))

  val pred_filter = Module(new LLCPredReqFilter(size = 6, name = "pred_filter"))
  val prefetcher = Module(new DummyLLCPrefetcher())
  val pft_filter = Module(new LLCPrefetchFilter(size = 6, name = "pft_filter"))
  val train_filter = Module(new LLCTrainFilter(size = 6, name = "train_filter"))

  pred_filter.io.enable := io.enable
  pred_filter.io.flush  := false.B
  pred_filter.io.ld_in  <> io_pred_in

  prefetcher.io.pred_req  <> pred_filter.io.pred_req
  prefetcher.io.pft_req   <> pft_filter.io.gen_req

  pft_filter.io.tlb_req   <> io.tlb_req
  pft_filter.io.pmp_resp  <> io.pmp_resp

  train_filter.io.ld_res  <> io.ld_in
  train_filter.io.pft_rec <> prefetcher.io.pft_rec
  train_filter.io.train_req <> prefetcher.io.train_req

  val is_valid_addr = PmemRanges.map(_.cover(pft_filter.io.llc_pf_addr.bits)).reduce(_ || _)
  io.l3_req.valid := pft_filter.io.llc_pf_addr.valid && is_valid_addr
  io.l3_req.bits  := pft_filter.io.llc_pf_addr.bits

  io.l1_req.valid := false.B
  io.l1_req.bits  := DontCare

  io.l2_req.valid := false.B
  io.l2_req.bits  := DontCare

}