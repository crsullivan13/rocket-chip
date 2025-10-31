// package freechips.rocketchip.subsystem

// import chisel3._
// import chisel3.util._
// import org.chipsalliance.cde.config.{Parameters}
// import freechips.rocketchip.diplomacy._
// import freechips.rocketchip.tilelink._
// import freechips.rocketchip.regmapper._
// //import midas.targetutils.SynthesizePrintf
// import org.chipsalliance.cde.config.{Parameters, Field, Config}

// import freechips.rocketchip.tile.BRUTileIO

// // BRUTileIO defined in BaseTile.scala so we have it everywhere

// class BwRegulator()(implicit p: Parameters) extends LazyModule
// {
//     val device = new SimpleDevice("bru",Seq("bru"))

//     // first number is number of cores, second is number of banks..
//     // TODO: Can we grab the number of cores from params somehow?
//     val ioNode = Seq.fill(4)(BundleBridgeSource(() => new BRUTileIO(p(SubsystemBankedCoherenceKey).nBanks)))
//     val dramRegNode = BundleBridgeSink[BRUTileIO](Some(() => Flipped(new BRUTileIO(4))))
//     val adapterNode = TLAdapterNode()

//     // add simple config registers
//     val regnode = new TLRegisterNode(
//         address = Seq(AddressSet(0x20000000, 0x7ff)),
//         device = device,
//         beatBytes = 8)

//     lazy val module = new BwRegulatorModule(this)
// }

// class BwRegulatorModule(outer: BwRegulator) extends LazyModuleImp(outer)
// {
//     val throttleIO = outer.ioNode.map(_.bundle)

//     val nDomains = 4

//     val adapterNode = outer.adapterNode
//     val nClients = adapterNode.in.length
//     println(s"Number of edges into BRU: $nClients")

//     val numBanks = p(SubsystemBankedCoherenceKey).nBanks
//     val numBankBits = log2Ceil(numBanks)

//     val cacheLineBits = log2Ceil(p(CacheBlockBytes))

//     val memBase = p(ExtMem).get.master.base.U

//     val globalEnable = RegInit(false.B)
//     val countInstrFetch = RegInit(true.B)

//     val wPeriod = 25
//     val w = wPeriod - 3
//     val periodCntr = Reg(UInt(wPeriod.W))
//     val periodLen = Reg(UInt(wPeriod.W))

//     val bankReadCntrs = Seq.fill(nDomains)(RegInit(VecInit(Seq.fill(numBanks)(0.U(w.W)))))
//     val maxReads = Reg(Vec(nDomains, UInt(w.W)))

//     val clientRegEnable = Reg(Vec(nClients, Bool()))
//     val clientDomainIds = Reg(Vec(nClients, UInt(log2Ceil(nDomains).W))) // which domain is a client in

//     val doesClientFireAcquire = Wire(Vec(nClients, Bool()))
//     val doesClientAccessBank = Seq.fill(nClients)(Wire(Vec(numBanks, Bool())))

//     // each domain has per-bank throttle signals
//     val domainReadThrottle = VecInit(Seq.fill(nDomains)(VecInit(Seq.fill(numBanks)(WireInit(Bool(), false.B)))))

//     val periodReset = periodCntr >= periodLen
//     periodCntr := Mux(periodReset || !globalEnable, 0.U, periodCntr + 1.U)

//     when ( periodReset ) {
//         //SynthesizePrintf(printf("Period reset\n"))
//     }

//     for ( i <- 0 until nDomains ) {
//         for ( j <- 0 until numBanks ) {
//             val clientDomainActive = ( clientDomainIds zip ( doesClientFireAcquire zip doesClientAccessBank ) ).map {
//                 case (domain, (active, bank)) => domain === i.U && active && bank(j)
//             }

//             when ( clientDomainActive.reduce(_||_) ) {
//                 //SynthesizePrintf(printf("Client %d accessing bank %d\n", i.U, j.U))
//                 // printf("Period cntr is %x\n", periodCntr)
//                 // printf("Global en is %x\n", globalEnable)
//                 // //SynthesizePrintf(printf("Count %x,%x is %x\n", i.U, j.U, bankReadCntrs(i)(j)))
//                 // printf("Should throttle %x\n", domainReadThrottle(i)(j))
//                 // printf("Period reset %x\n", periodReset)
//                 // printf("Inc value %x\n", clientDomainActive.reduce(_||_) + Mux(periodReset, 0.U, bankReadCntrs(i)(j)))
//             }

//             bankReadCntrs(i)(j) := Mux(globalEnable, clientDomainActive.reduce(_||_) + Mux(periodReset, 0.U, bankReadCntrs(i)(j)), 0.U)

//             domainReadThrottle(i)(j) := Mux(globalEnable, bankReadCntrs(i)(j) >= maxReads(i), 0.B)
//         }
//     }

//     for ( i <- 0 until nClients ) {
//         val (out, edge_out) = adapterNode.out(i)
//         val (in, edge_in) = adapterNode.in(i)

//         out <> in

//         val isAcquire = in.a.bits.opcode === TLMessages.AcquireBlock
//         val isInstrFetch = in.a.bits.opcode === TLMessages.Get && in.a.bits.address >= memBase

//         val isAccessRead = isAcquire || ( countInstrFetch && isInstrFetch )

//         out.a.bits.domainId := clientDomainIds(i)
//         out.c.bits.domainId := clientDomainIds(i)

//         doesClientFireAcquire(i) := isAccessRead && in.a.fire && clientRegEnable(i)

//         for ( j <- 0 until numBanks ) {
//             if ( numBanks > 1 ) {
//                 doesClientAccessBank(i)(j) := in.a.bits.address(cacheLineBits + numBankBits-1, cacheLineBits) === j.U
//             } else{
//                 doesClientAccessBank(i)(j) := 1.U
//             }
//         }

//         for ( j <- 0 until numBanks ) {
//             //throttleIO(i).nThrottle(j) := domainReadThrottle(clientDomainIds(i))(j) && clientRegEnable(i) && globalEnable
//             throttleIO(i).nThrottle(j) := false.B
//         }

//         when ( clientRegEnable(i) && globalEnable ) {
//             for ( j <- 0 until numBanks ) {
//                 //throttleIO(i).nThrottle(j) := false.B
//                 when ( ( domainReadThrottle(clientDomainIds(i))(j) && doesClientAccessBank(i)(j) && isAccessRead ) || outer.dramRegNode.bundle.nThrottle(clientDomainIds(i)) ) {
//                     //SynthesizePrintf(printf("Throttling client %x bank %x\n", i.U, j.U))
//                     //SynthesizePrintf(printf("Count %x,%x is %x\n", i.U, j.U, bankReadCntrs(i)(j)))
//                     in.a.ready := false.B
//                     out.a.valid := false.B
//                     //throttleIO(i).nThrottle(j) := true.B
//                 }
//             }
//         }
//     }

//     val globalEnableRegField = Seq(0 -> Seq(
//         RegField(globalEnable.getWidth, globalEnable,
//             RegFieldDesc("globalEnable", "Toggle entire unit"))))

//     val settingsRegField = Seq(8 -> Seq (
//         RegField(countInstrFetch.getWidth, countInstrFetch,
//             RegFieldDesc("countInstrFetch", "Toggle fetch counting"))))

//     val periodLenRegField = Seq(16 -> Seq (
//         RegField(periodLen.getWidth, periodLen,
//             RegFieldDesc("periodLen", "Set regulation period"))))

//     val maxReadsRegField = maxReads.zipWithIndex.map { case (register, i) =>
//         (24 + i * 8) -> Seq(RegField(register.getWidth, register,
//             RegFieldDesc(s"maxRead$i", s"Max reads for domain $i")))}

//     val clientRegEnableRegField = Seq((24 + nDomains * 8) -> clientRegEnable.zipWithIndex.map { case (client, i) =>
//         RegField(client.getWidth, client, RegFieldDesc(s"client${i}En", s"Reg enable for client$i")) })

//     val domainIdField = clientDomainIds.zipWithIndex.map { case(domain, i) =>
//         (48 + nDomains * 8 + i * 8) -> Seq(RegField(domain.getWidth, domain, RegFieldDesc(s"domainId$i", s"Client $i domain ID"))) }

//     val mmioReg = globalEnableRegField ++ settingsRegField ++ periodLenRegField ++ maxReadsRegField ++ clientRegEnableRegField ++ domainIdField

//     outer.regnode.regmap(mmioReg: _*)
// }

// trait CanHavePeripheryBRU {
//     val BwRegulator: Option[BwRegulator]
// }

// trait CanHaveBRU { this: BaseSubsystem =>
//     private val pbus = locateTLBusWrapper(PBUS)
//     private val sbus = locateTLBusWrapper(SBUS)

//     private val portName = "bru-mmio"

//     sbus.BwRegulator.map { bwreg => 
//         pbus.coupleTo(portName) {
//             bwreg.regnode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ 
//         }
//     }
// }