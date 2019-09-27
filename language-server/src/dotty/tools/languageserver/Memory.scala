package dotty.tools
package languageserver

object Memory {

  /** Memory is judged to be critical if after a GC the amount of used memory
   *  divided by total available memory exceeds this threshold.
   */
  val UsedThreshold = 0.9

  /** If total available memory is unknown, memory is judged to be critical if
   *  after a GC free memory divided by used memory is under this threshold.
   */
  val FreeThreshold = 0.1

  /** Turn this flag on to stress test restart capability in compiler.
   *  It will restart the presentation compiler after every 10 editing actions
   */
  private final val stressTest = false
  private var stressTestCounter = 0

  /** Is memory critically low? */
  def isCritical(): Boolean = {
    if (stressTest) {
      stressTestCounter += 1
      if (stressTestCounter % 10 == 0) return true
    }
    val runtime = Runtime.getRuntime
    def total = runtime.totalMemory
    def maximal = runtime.maxMemory
    def free = runtime.freeMemory
    def used = total - free
    def usedIsCloseToMax =
      if maximal == Long.MaxValue then free.toDouble / used < FreeThreshold
      else used.toDouble / maximal > UsedThreshold
    usedIsCloseToMax && { runtime.gc(); usedIsCloseToMax }
  }

  def stats(): String = {
    val M = 2 << 20
    val runtime = Runtime.getRuntime
    def total = runtime.totalMemory / M
    def maximal = runtime.maxMemory / M
    def free = runtime.freeMemory / M
    s"total used memory: $total MB, free: $free MB, maximal available = $maximal MB"
  }
}
