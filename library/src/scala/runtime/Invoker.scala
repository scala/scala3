package scala.runtime

import scala.collection.mutable.{BitSet, AnyRefMap}
import scala.collection.concurrent.TrieMap
import java.nio.file.Files
import java.io.FileWriter
import java.io.File

object Invoker {
  private val runtimeUUID = java.util.UUID.randomUUID()

  private val MeasurementsPrefix = "scoverage.measurements."
  private val threadFiles = new ThreadLocal[AnyRefMap[String, FileWriter]]
  private val dataDirToSet = TrieMap.empty[String, BitSet]

  /** We record that the given id has been invoked by appending its id to the coverage data file.
    *
    * This will happen concurrently on as many threads as the application is using, so we use one
    * file per thread, named for the thread id.
    *
    * This method is not thread-safe if the threads are in different JVMs, because the thread IDs
    * may collide. You may not use `scoverage` on multiple processes in parallel without risking
    * corruption of the measurement file.
    *
    * @param id
    *   the id of the statement that was invoked
    * @param dataDir
    *   the directory where the measurement data is held
    */
  def invoked(id: Int, dataDir: String): Unit =
    val set = dataDirToSet.getOrElseUpdate(dataDir, BitSet.empty)
    if !set.contains(id) then
      val added = set.synchronized {
        set.add(id)
      }
      if added then
        var writers = threadFiles.get()
        if writers == null then
          writers = AnyRefMap.empty
          threadFiles.set(writers)
        val writer = writers.getOrElseUpdate(
          dataDir,
          FileWriter(measurementFile(dataDir), true)
        )
        writer.write(Integer.toString(id))
        writer.write('\n')
        writer.flush()

  def measurementFile(dataDir: String): File = new File(
    dataDir,
    MeasurementsPrefix + runtimeUUID + "." + Thread.currentThread.nn.getId
  )
}
