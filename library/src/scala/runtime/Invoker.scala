package scala.runtime

import scala.collection.mutable
import java.nio.file.Path
import scala.collection.concurrent.TrieMap
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import java.io.FileWriter
import java.io.File

object Invoker {
  private val runtimeUUID = java.util.UUID.randomUUID()

  private val MeasurementsPrefix = "scoverage.measurements."
  private val threadFiles = new ThreadLocal[mutable.HashMap[String, FileWriter]]

  // For each data directory we maintain a thread-safe set tracking the ids
  // that we've already seen and recorded. We're using a map as a set, so we
  // only care about its keys and can ignore its values.
  private val dataDirToIds = TrieMap.empty[String, TrieMap[Int, Any]]

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
  def invoked(id: Int, dataDir: String): Unit = {
    // [sam] we can do this simple check to save writing out to a file.
    // This won't work across JVMs but since there's no harm in writing out the same id multiple
    // times since for coverage we only care about 1 or more, (it just slows things down to
    // do it more than once), anything we can do to help is good. This helps especially with code
    // that is executed many times quickly, eg tight loops.
    if (!dataDirToIds.contains(dataDir)) {
      // Guard against SI-7943: "TrieMap method getOrElseUpdate is not thread-safe".
      dataDirToIds.synchronized {
        if (!dataDirToIds.contains(dataDir)) {
          dataDirToIds(dataDir) = TrieMap.empty[Int, Any]
        }
      }
    }
    val ids = dataDirToIds(dataDir)
    if (!ids.contains(id)) {
      // Each thread writes to a separate measurement file, to reduce contention
      // and because file appends via FileWriter are not atomic on Windows.
      var files = threadFiles.get()
      if (files == null) {
        files = mutable.HashMap.empty[String, FileWriter]
        threadFiles.set(files)
      }
      val writer = files.getOrElseUpdate(
        dataDir,
        new FileWriter(measurementFile(dataDir), true)
      )

      writer.append(Integer.toString(id))
      writer.append("\n")
      writer.flush()
      ids.put(id, ())
    }
  }

  def measurementFile(dataDir: String): File = new File(
    dataDir,
    MeasurementsPrefix + runtimeUUID + "." + Thread.currentThread.nn.getId
  )
}
