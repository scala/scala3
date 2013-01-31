package dotty.tools
package io

import java.util.concurrent._

class DaemonThreadFactory extends ThreadFactory {
  def newThread(r: Runnable): Thread = {
    val thread = new Thread(r)
    thread setDaemon true
    thread
  }
}

object DaemonThreadFactory {
  def newPool() = Executors.newCachedThreadPool(new DaemonThreadFactory)
}
