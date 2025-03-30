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
  val retryBufSize = 72
  val reqBufSize = 72
}

class LLCPrefetcherModule()(implicit p: Parameters) extends XSModule with HasLLCPrefetcherParam

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

class LLCPrefetchTask()(implicit p: Parameters) extends XSBundle{
  val uop = new DynInst
  val pc    = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)

  val is_repeated = Bool()

  // for retry
  val retry_base = UInt(2.W)    // Range: [1, 3]
  val origin_res = Bool()       // 0: not issue pft originally, 1: issue pft originally

  def fromMemExuInput(input: MemExuInput) = {
    this  := DontCare
    this.uop    := input.uop
    this.vaddr  := input.src(0) + SignExt(input.uop.imm(11, 0), VAddrBits)
    this.pc     := input.uop.pc
  }
}

class LLCPrefetchRecord()(implicit p: Parameters) extends XSBundle{
  val uop = new DynInst
  val vaddr = UInt(VAddrBits.W)
  val feat_idx = new FeatureBundle()
  val above_threshold = Bool()    // predictor result
  val issue_prefetch = Bool()     // final prefetch result
}

class ReqBuf()(implicit p: Parameters) extends LLCPrefetcherModule with HasLLCFilterHelper with HasLLCPrefetchHelper {
  val io = IO(new Bundle{
    val enable = Input(Bool())
    val redirect = Flipped(Valid(new Redirect))
    val ld_in = Flipped(Vec(backendParams.LduCnt, ValidIO(new MemExuInput())))

    val mainPipe_info = Flipped(Vec(2, ValidIO(UInt(VAddrBits.W))))
    val retryBuf_info = Flipped(Vec(retryBufSize, ValidIO(UInt(VAddrBits.W))))

    val req_task = Decoupled(new LLCPrefetchTask())
  })

  val (mainPipe_info, retryBuf_info) = (io.mainPipe_info, io.retryBuf_info)

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => reqBufSize ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = Reg(Vec(reqBufSize, new LLCPrefetchTask()))
  val valids  = RegInit(VecInit(Seq.fill(reqBufSize){false.B}))
  val is_flushed = RegInit(VecInit(Seq.fill(reqBufSize){false.B}))

  // enq
  val enqLen = backendParams.LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))

  val deqPtr = WireInit(deqPtrExt.value)

  require(reqBufSize >= enqLen)

  val ld_in_reordered = reorder(io.ld_in)
  val reqs_l = Wire(Vec(enqLen, new LLCPrefetchTask()))
  val reqs_vl = ld_in_reordered.map(_.valid)
  for (i <- 0 until enqLen) {
    reqs_l(i).fromMemExuInput(ld_in_reordered(i).bits)
  }

  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))

  val use_mainpipe_info = Constantin.createRecord("useMainPipeFwd", true)
  val use_retrybuf_info = Constantin.createRecord("useRetryBufFwd", true)

  for (i <- 0 until enqLen) {
    val req = reqs_l(i)
    val req_v = reqs_vl(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)

    val reqbuf_match = Cat(entries.zip(valids).zip(is_flushed).map {
      case ((e, v), f) => v && block_addr(e.vaddr) === block_addr(req.vaddr) && !f
    }).orR

    val prev_enq_match = if(i == 0) false.B else Cat(reqs_l.zip(reqs_vl).take(i).map {
      case(pre, pre_v) => pre_v && block_addr(pre.vaddr) === block_addr(req.vaddr)
    }).orR

    val mainpipe_match = Cat(mainPipe_info.map(e => e.valid && block_addr(e.bits) === block_addr(req.vaddr))).orR && use_mainpipe_info
    val retrybuf_match = Cat(retryBuf_info.map(e => e.valid && block_addr(e.bits) === block_addr(req.vaddr))).orR && use_retrybuf_info

    needAlloc(i)  := req_v && !reqbuf_match && !prev_enq_match && !req.uop.robIdx.needFlush(io.redirect)
    canAlloc(i)   := needAlloc(i) && allocPtr >= deqPtrExt && io.enable

    XSPerfAccumulate(s"FailtoEnqReqBuf$i", needAlloc(i) && allocPtr < deqPtrExt && io.enable)

    when (canAlloc(i)) {
      valids(allocPtr.value) := true.B

      entries(allocPtr.value) := req
      entries(allocPtr.value).is_repeated := mainpipe_match || retrybuf_match

      is_flushed(allocPtr.value)  := false.B
    }
  }

  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach(x => when(canAlloc.asUInt.orR) {x := x + allocNum})

  for (i <- 0 until reqBufSize) {
    when (valids(i) && entries(i).uop.robIdx.needFlush(io.redirect)){
      is_flushed(i) := true.B
    }
  }

  // deq
  io.req_task.valid := false.B
  io.req_task.bits  := DontCare
  for (i <- 0 until reqBufSize) {
    when (deqPtr === i.U){
      io.req_task.valid := valids(i) && io.enable && !is_flushed(i) && !entries(i).uop.robIdx.needFlush(io.redirect)
      io.req_task.bits  := entries(i)
    }
  }

  when (io.req_task.fire) {
    valids(deqPtr)  := false.B
    deqPtrExt       := deqPtrExt + 1.U
  }

  when (valids(deqPtr) && is_flushed(deqPtr)) {
    is_flushed(deqPtr)  := false.B
    valids(deqPtr)      := false.B
    deqPtrExt := deqPtrExt + 1.U
  }
}

class MainPipe()(implicit p: Parameters) extends LLCPrefetcherModule{
  val io = IO(new Bundle{
    val redirect = Flipped(Valid(new Redirect))
    val req_task = Flipped(ValidIO(new LLCPrefetchTask()))

    // to Predictor
    val predictor_req = ValidIO(new LLCPrefetchTask())
    val predictor_rsp = Flipped(ValidIO(new LLCPrefetchRecord()))

    // to VirtualLoadQueue
    val record_req = ValidIO(new LLCPrefetchRecord())

    // to ReqBuf
    val mainPipe_info = Vec(2, ValidIO(UInt(VAddrBits.W)))

    // to RetryBuf
    val retry_req = ValidIO(new LLCPrefetchTask())

    // to TLB
    val tlb_req = new TlbRequestIO(2)
    val pmp_rsp = Flipped(new PMPRespBundle())

    // issue req
    val llc_pf_req = ValidIO(UInt(PAddrBits.W))
  })

  val (tlb_req, tlb_rsp, pmp_rsp) = (io.tlb_req.req, io.tlb_req.resp, io.pmp_rsp)
  io.tlb_req.req_kill := false.B

  val (prd_req, prd_rsp) = (io.predictor_req, io.predictor_rsp)

  /**
   *  Stage 1
   *  1. Issue TLB Req
   *  2. Issue Prediction Req
   * */

  val s1_task = io.req_task.bits
  val s1_valid = io.req_task.valid && !s1_task.uop.robIdx.needFlush(io.redirect)

  // Req TLB
  tlb_req.valid := s1_valid
  tlb_req.bits  := DontCare
  tlb_req.bits.vaddr      := s1_task.vaddr
  tlb_req.bits.cmd        :=  TlbCmd.read
  tlb_req.bits.isPrefetch := true.B
  tlb_req.bits.size       := 3.U

  // Req Predictor
  prd_req.valid := s1_valid
  prd_req.bits  := s1_task

  /**
   * Stage 2
   * 1. Check TLB Rsp
   * 2. Check Predictor Rsp
   * 3. TLB hit: commit record, maybe issue prefetch req
   * 4. TLB miss: send to retry buf
   * */

  val s2_task = RegNext(s1_task)
  val s2_valid  = RegNext(s1_valid) && !s2_task.uop.robIdx.needFlush(io.redirect)

  // tlb
  tlb_rsp.ready := true.B
  val tlb_miss = tlb_rsp.bits.miss
  val tlb_excp = tlb_rsp.bits.excp.head.pf.ld || tlb_rsp.bits.excp.head.gpf.ld || tlb_rsp.bits.excp.head.af.ld ||  // page/access fault
    pmp_rsp.mmio || Pbmt.isUncache(tlb_rsp.bits.pbmt.head) || // uncache
    pmp_rsp.ld

  val paddr = Cat(tlb_rsp.bits.paddr.head(PAddrBits - 1, log2Ceil(64)), 0.U(log2Ceil(64).W))
  val paddr_valid = PmemRanges.map(_.cover(paddr)).reduce(_ || _)

  // predictor
  val prd_issue_pft = prd_rsp.valid && prd_rsp.bits.above_threshold

  // llc req
  io.llc_pf_req.valid := s2_valid && !tlb_miss && !tlb_excp && prd_issue_pft && paddr_valid && !s2_task.is_repeated
  io.llc_pf_req.bits  := paddr
  XSPerfAccumulate("NotIssueRepeatedTask", s2_valid && !tlb_miss && !tlb_excp && prd_issue_pft && paddr_valid && s2_task.is_repeated)

  // commit record
  io.record_req.valid := s2_valid && !tlb_miss
  io.record_req.bits  := prd_rsp.bits
  io.record_req.bits.issue_prefetch := io.llc_pf_req.valid

  // to RetryBuf
  io.retry_req.valid  := s2_valid && tlb_miss
  io.retry_req.bits   := s2_task
  io.retry_req.bits.retry_base  := Mux(s2_task.retry_base < 3.U, s2_task.retry_base + 1.U, 3.U)
  io.retry_req.bits.origin_res  := Mux(s2_task.retry_base === 0.U, prd_issue_pft, s2_task.origin_res)
  XSPerfAccumulate("PredictionDiff", prd_issue_pft =/= io.retry_req.bits.origin_res && s2_valid)

  // info
  io.mainPipe_info(0).valid := s1_valid
  io.mainPipe_info(0).bits  := s1_task.vaddr
  io.mainPipe_info(1).valid := s2_valid
  io.mainPipe_info(1).bits  := s2_task.vaddr
}

class RetryBuf()(implicit p: Parameters) extends LLCPrefetcherModule with HasLLCFilterHelper with HasLLCPrefetchHelper{
  val io = IO(new Bundle{
    val redirect = Flipped(Valid(new Redirect))
    val retry_req_enq = Flipped(ValidIO(new LLCPrefetchTask()))
    val retry_req_deq = Decoupled(new LLCPrefetchTask())
    val retryBuf_info = Vec(retryBufSize, ValidIO(UInt(VAddrBits.W)))
  })

  XSPerfAccumulate("RetryBufEnqTime0", io.retry_req_enq.valid && io.retry_req_enq.bits.retry_base === 0.U)
  XSPerfAccumulate("RetryBufEnqTime1", io.retry_req_enq.valid && io.retry_req_enq.bits.retry_base === 1.U)
  XSPerfAccumulate("RetryBufEnqTime2", io.retry_req_enq.valid && io.retry_req_enq.bits.retry_base === 2.U)
  XSPerfAccumulate("RetryBufEnqTime3", io.retry_req_enq.valid && io.retry_req_enq.bits.retry_base === 3.U)
  XSPerfAccumulate("RetryBufDeq", io.retry_req_deq.fire)

  val entries = RegInit(VecInit(Seq.fill(retryBufSize)(0.U.asTypeOf(new LLCPrefetchTask()))))
  val valids = RegInit(VecInit(Seq.fill(retryBufSize)(false.B)))
  val timers = RegInit(VecInit(Seq.fill(retryBufSize)(0.U(4.W))))

  // redirect check
  val redirect_vec = entries.zip(valids).map{
    case (e, v) =>
      v && e.uop.robIdx.needFlush(io.redirect)
  }

  for (i <- 0 until retryBufSize) {
    when (redirect_vec(i)) {
      valids(i) := false.B
    }
  }

  // timer
  for (i <- 0 until retryBufSize) {
    when (valids(i)) {
      timers(i) := Mux(timers(i) > 1.U, timers(i) - 1.U, 0.U)
    }
  }

  // infos
  for (i <- 0 until retryBufSize) {
    io.retryBuf_info(i).valid := valids(i) && !redirect_vec(i)
    io.retryBuf_info(i).bits  := entries(i).vaddr
  }

  // enq logic
  val enq_req = io.retry_req_enq

  val availableVec = VecInit(valids.map(e => !e))
  val canAlloc = Cat(availableVec).orR
  val availableOH = PriorityEncoderOH(availableVec)
  val allocIdx = OHToUInt(availableOH)

  val entry_hit_vec = entries.zip(valids).map{
    case (e, v) =>
      v && block_addr(e.vaddr) === block_addr(enq_req.bits.vaddr)
  }
  val entry_hit = Cat(entry_hit_vec).orR
  XSPerfAccumulate("EnqReqDuplicated", enq_req.valid && entry_hit)

  val needAlloc = enq_req.valid && canAlloc && !enq_req.bits.uop.robIdx.needFlush(io.redirect)
  XSPerfAccumulate("FailToEnqRetryBuf", enq_req.valid && !canAlloc)

  when (needAlloc){
    entries(allocIdx) := enq_req.bits
    valids(allocIdx)  := true.B
    timers(allocIdx)  := (1.U << enq_req.bits.retry_base)
  }

  // deq logic
  val deq_arb = Module(new RRArbiterInit(new LLCPrefetchTask(), retryBufSize))

  for (i <- 0 until retryBufSize) {
    deq_arb.io.in(i).valid  := valids(i) && timers(i) === 0.U && !redirect_vec(i)
    deq_arb.io.in(i).bits   := entries(i)

    when (deq_arb.io.in(i).fire) {
      valids(i) := false.B
    }
  }

  deq_arb.io.out  <> io.retry_req_deq

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

class LLCTrainFilter(size: Int = 16, name: String)(implicit p: Parameters) extends XSModule with HasTrainFilterHelper with HasLLCPrefetchHelper {
  val io = IO(new Bundle{
    val ld_res = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LsPrefetchTrainBundle))) // max 3/cycle
    val pft_rec = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LLCPrefetchRecord()))) // max 3/cycle
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
    train_req(i).bits.need_dec  := !res(i).bits.is_offchip && rec(i).bits.issue_prefetch && rec(i).bits.issue_prefetch === rec(i).bits.above_threshold
    train_req(i).bits.need_inc  := res(i).bits.is_offchip && !rec(i).bits.issue_prefetch && rec(i).bits.issue_prefetch === rec(i).bits.above_threshold
    train_req(i).bits.feat_idx  := rec(i).bits.feat_idx

    XSPerfAccumulate(s"OffchipLoadTrain$i",  train_req(i).valid && train_req(i).bits.need_inc)
    XSPerfAccumulate(s"NonOffchipLoadTrain$i", train_req(i).valid && train_req(i).bits.need_dec)
    XSPerfAccumulate(s"OffchipLoadTrainDrop$i", train_req(i).valid && train_req(i).bits.need_inc && drop)
    XSPerfAccumulate(s"NonOffchipLoadTrainDrop$i", train_req(i).valid && train_req(i).bits.need_dec && drop)
  }

  for (i <- 0 until backendParams.LdExuCnt) {
    val v = res(i).valid

    io.upt_pc(i).valid  := v
    io.upt_pc(i).bits   := res(i).bits.uop.pc

    io.upt_vaddr(i).valid := v
    io.upt_vaddr(i).bits  := res(i).bits.vaddr
  }

  // PerfCnt
  for (i <- 0 until backendParams.LdExuCnt){
    val v = res(i).valid && rec(i).valid

    XSPerfAccumulate(s"correctLLCPrefetch$i", v && res(i).bits.is_offchip === rec(i).bits.issue_prefetch)
    XSPerfAccumulate(s"incorrectLLCPrefetch$i", v && res(i).bits.is_offchip =/= rec(i).bits.issue_prefetch)
    XSPerfAccumulate(s"correctOffchipLLCPrefetch$i", v && res(i).bits.is_offchip === rec(i).bits.issue_prefetch && res(i).bits.is_offchip)
    XSPerfAccumulate(s"incorrectOffchipLLCPrefetch$i", v && res(i).bits.is_offchip =/= rec(i).bits.issue_prefetch && res(i).bits.is_offchip)
  }
}

class BaseLLCPrefetcher()(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle(){
    val prd_req = Flipped(ValidIO(new LLCPrefetchTask()))
    val prd_rsp = ValidIO(new LLCPrefetchRecord())
    val train_req = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LLCTrainReq())))
  })
}

class PerceptronLLCPrefetcher(last_pc_num: Int = 4)(implicit p: Parameters) extends BaseLLCPrefetcher with HasLLCPrefetcherParam {
  val io_upt_vaddr  = IO(Flipped(Vec(backendParams.LdExuCnt, ValidIO(UInt(VAddrBits.W)))))    // for training
  val io_upt_pc     = IO(Flipped(Vec(backendParams.LdExuCnt, ValidIO(UInt(VAddrBits.W)))))    // for training

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

  val s1_req = io.prd_req
  val s2_req = RegNext(s1_req)

  // Access Table
  val pbuffer = Module(new PageBuffer(AddrBits = VAddrBits, TrainPortNum = backendParams.LdExuCnt))
  pbuffer.io.req_addr   := s1_req.bits.vaddr
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
  val init_val = Constantin.createRecord("weightInitVal", 0)
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
  val inc_delta = correct_cnt - incorrect_cnt
  val dec_delta = incorrect_cnt - correct_cnt


  /*------------------------------ s1 ------------------------------*/
  val s1_valid = s1_req.valid

  // Calculate Features
  val vaddr = s1_req.bits.vaddr
  val pc    = s1_req.bits.pc

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
  val feat_idx_reg = RegNext(feat_idx)

  val s2_valid  = RegNext(s1_valid)

  val tau_act = Constantin.createRecord("LLCPredictorActThreshold", -18)
  val sum = weights.reduce(_ + _)
  val above_threshold = sum > tau_act.asSInt
  dontTouch(sum)

  io.prd_rsp.valid  := s2_valid
  io.prd_rsp.bits   := DontCare
  io.prd_rsp.bits.uop             := s2_req.bits.uop
  io.prd_rsp.bits.vaddr           := s2_req.bits.vaddr
  io.prd_rsp.bits.above_threshold := above_threshold
  io.prd_rsp.bits.feat_idx.connect_from_vec(feat_idx_reg)

}

class LLCPrefetcher()(implicit p: Parameters) extends BasePrefecher {
  val io_pred_in = IO(Flipped(Vec(backendParams.LdExuCnt, ValidIO(new MemExuInput))))
  val io_rec_req = IO(ValidIO(new LLCPrefetchRecord()))
  val io_rec_rsp = IO(Vec(backendParams.LdExuCnt, Flipped(ValidIO(new LLCPrefetchRecord()))))
  val io_redirect = IO(Flipped(Valid(new Redirect)))
  val io_l1_pf_req   = IO(Flipped(ValidIO(UInt(PAddrBits.W))))
  val io_sms_pf_req  = IO(Flipped(ValidIO(UInt(PAddrBits.W))))

  val reqBuf    = Module(new ReqBuf())
  val mainPipe  = Module(new MainPipe())
  val retryBuf  = Module(new RetryBuf())

  val train_filter = Module(new LLCTrainFilter(name = "train_filter"))

  val perceptron_prefetcher = Module(new PerceptronLLCPrefetcher())

  // redirect
  val use_redirect = Constantin.createRecord("llcUseRedirect", true)

  reqBuf.io.redirect.valid    := io_redirect.valid && use_redirect
  reqBuf.io.redirect.bits     := io_redirect.bits
  mainPipe.io.redirect.valid  := io_redirect.valid && use_redirect
  mainPipe.io.redirect.bits   := io_redirect.bits
  retryBuf.io.redirect.valid  := io_redirect.valid && use_redirect
  retryBuf.io.redirect.bits   := io_redirect.bits

  reqBuf.io.enable          := io.enable
  reqBuf.io.ld_in           <> io_pred_in
  reqBuf.io.req_task.ready  := !retryBuf.io.retry_req_deq.valid
  reqBuf.io.retryBuf_info   <> retryBuf.io.retryBuf_info
  reqBuf.io.mainPipe_info   <> mainPipe.io.mainPipe_info

  retryBuf.io.retry_req_deq.ready := true.B

  mainPipe.io.req_task.valid  := reqBuf.io.req_task.valid || retryBuf.io.retry_req_deq.valid
  mainPipe.io.req_task.bits   := Mux(retryBuf.io.retry_req_deq.valid, retryBuf.io.retry_req_deq.bits, reqBuf.io.req_task.bits)
  mainPipe.io.tlb_req         <> io.tlb_req
  mainPipe.io.pmp_rsp         <> io.pmp_resp
  mainPipe.io.record_req      <> io_rec_req
  mainPipe.io.predictor_req   <> perceptron_prefetcher.io.prd_req
  mainPipe.io.predictor_rsp   <> perceptron_prefetcher.io.prd_rsp
  mainPipe.io.retry_req       <> retryBuf.io.retry_req_enq

  train_filter.io.ld_res      <> io.ld_in
  train_filter.io.pft_rec     <> io_rec_rsp
  train_filter.io.train_req   <> perceptron_prefetcher.io.train_req
  train_filter.io.upt_pc      <> perceptron_prefetcher.io_upt_pc
  train_filter.io.upt_vaddr   <> perceptron_prefetcher.io_upt_vaddr

  val llc_pf_req = mainPipe.io.llc_pf_req

  // magic?
  val pft_pbuffer = Module(new PageBuffer(AddrBits = PAddrBits, TrainPortNum = 2))
  pft_pbuffer.io.req_addr := llc_pf_req.bits

  pft_pbuffer.io.train_addr(0).valid  := io_l1_pf_req.valid
  pft_pbuffer.io.train_addr(0).bits   := io_l1_pf_req.bits

  pft_pbuffer.io.train_addr(1).valid  := io_sms_pf_req.valid
  pft_pbuffer.io.train_addr(1).bits   := io_sms_pf_req.bits

  val use_pft_acc_tab = Constantin.createRecord("usePftAccTab", false)
  val pft_acc_tab_allow = !pft_pbuffer.io.is_accessed && use_pft_acc_tab || !use_pft_acc_tab

  io.l3_req.valid := llc_pf_req.valid && pft_acc_tab_allow
  io.l3_req.bits  := llc_pf_req.bits

  io.l1_req.valid := false.B
  io.l1_req.bits  := DontCare

  io.l2_req.valid := false.B
  io.l2_req.bits  := DontCare
}