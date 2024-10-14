// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  dtsFrequency: Option[BigInt] = None,
  zeroDevice: Option[BuiltInZeroDeviceParams] = None,
  errorDevice: Option[BuiltInErrorDeviceParams] = None,
  replication: Option[ReplicatedRegion] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with HasRegionReplicatorParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): MemoryBus = {
    val mbus = LazyModule(new MemoryBus(this, loc.name, context))
    mbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> mbus)
    mbus
  }
}

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams, name: String = "memory_bus", context: HasTileLinkLocations)(implicit p: Parameters)
    extends TLBusWrapper(params, name)(p)
{
  private val replicator = params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  //val memcount = None
  val pbus = context.tlBusWrapperLocationMap(PBUS)
  val memcount = Some(LazyModule(new MemCounter(params = BRUParams(0x20001000L,2,0,0,false))))
  pbus.coupleTo("dram-count-bru") { 
    memcount.get.regnode := 
    TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }

  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")
  val inwardNode: TLInwardNode =
    replicator.map(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all) :*=* _.node)
        .getOrElse(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all))

  // potentially add DRAM BRU here
  val outwardNode: TLOutwardNode = memcount.get.node := ProbePicker() :*= xbar.node
  def busView: TLEdge = xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
