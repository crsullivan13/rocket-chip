// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._

/** Deterministic Memory (DM) request attribute.
  *
  * Farshchi, Valsan, Mancuso, Yun, "Deterministic Memory Abstraction and
  * Supporting Multicore System Architecture", ECRTS 2018.
  *
  * A request marked `dm` targets a memory region the OS has declared
  * deterministic; the shared cache uses the bit to drive the enhanced
  * way-partitioning replacement policy (see the InclusiveCache Directory).
  *
  * This travels in `TLBundleA.user` / `TLBundleB.user`, so a master that does
  * not declare `DMField()` in its `requestFields` simply produces `None` from
  * `user.lift(DMKey)` at the cache, which degrades to `dm = false` -- i.e.
  * exactly baseline behavior.
  */
case object DMKey extends ControlKey[Bool]("dm")

case class DMField() extends SimpleBundleField(DMKey)(Output(Bool()), false.B)
