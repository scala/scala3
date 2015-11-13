package dotty.tools
package dotc.util

trait CheckedSingleThreaded {

    /** The thread on which `checkSingleThreaded was invoked last */
    @sharable @volatile private var thread: Thread = null

    /** Check that we are on the same thread as before */
    def checkSingleThreaded() =
      if (thread == null) thread = Thread.currentThread()
      else assert(thread == Thread.currentThread(), s"illegal multithreaded access to $this")

}