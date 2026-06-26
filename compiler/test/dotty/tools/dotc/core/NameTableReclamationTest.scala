package dotty.tools.dotc.core

import Names.*

import org.junit.Test
import org.junit.Assert.*

import java.lang.ref.WeakReference

/** Regression test for issue #1584: names interned into the (global) name table must be reclaimable
 *  by GC once nothing references them. A strong, never-releasing table keeps them alive and fails
 *  this. Tracked with weak sentinels, so the test is independent of names other tests intern into
 *  the shared table.
 */
class NameTableReclamationTest:

  /** Intern `n` distinct names and return weak sentinels to a sample of them. The only strong
   *  references are local to this method, so they are gone once it returns. */
  private def internSample(n: Int): List[WeakReference[AnyRef]] =
    val live = Array.tabulate(n)(i => termName(s"i1584_reclaim_$i"))
    List(0, n / 2, n - 1).map(i => new WeakReference[AnyRef](live(i)))

  @Test def internedNamesAreReclaimed(): Unit =
    val sentinels = internSample(20_000)
    // GC until the sampled names are collected, bounded so a leak fails the test instead of hanging.
    var tries = 0
    while sentinels.exists(_.get != null) && tries < 100 do
      System.gc(); Thread.sleep(10); tries += 1
    assertTrue("interned names must be collectable once unreferenced (the leak in #1584)",
      sentinels.forall(_.get == null))
