package dotty.tools
package dotc
package ast

import core.Contexts.SourceInfo
import io.AbstractFile
import scala.annotation.internal.sharable
import scala.collection.mutable
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

object TreeIds {

  private final val ChunkSize = 1024

  @sharable private[this] val counters = new ConcurrentHashMap[AbstractFile, AtomicInteger]
  @sharable private[this] val fileOfChunk = mutable.ArrayBuffer[AbstractFile]()

  def nextIdFor(file: AbstractFile)(implicit src: SourceInfo): Int = {
    def getCounter: AtomicInteger =
      if (file `eq` src.srcfile) {
        var cachedCtr = src.counter
        if (cachedCtr == null) {
          cachedCtr = counters.get(file)
          src.counter = cachedCtr
        }
        cachedCtr
      }
      else counters.get(file)
    var ctr = getCounter
    if (ctr == null) {
      counters.putIfAbsent(file, new AtomicInteger)
      ctr = getCounter
    }
    def recur(): Int = {
      val id = ctr.get
      if (id % ChunkSize == 0) newChunk(file, ctr)
      else if (ctr.compareAndSet(id, id + 1)) id
      else recur()
    }
    recur()
  }

  private def newChunk(file: AbstractFile, ctr: AtomicInteger): Int = synchronized {
    val id = fileOfChunk.length * ChunkSize
    fileOfChunk += file
    ctr.set(id + 1)
    id
  }

  def fileOfId(id: Int): AbstractFile =
    fileOfChunk(id / ChunkSize)
}