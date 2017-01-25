package transactional
import collection.mutable.ListBuffer

class Transaction {
  private val log = new ListBuffer[String]
  def println(s: String): Unit = log += s

  private var aborted = false
  private var committed = false

  def abort(): Unit = { aborted = true }
  def isAborted = aborted

  def commit(): Unit =
    if (!aborted && !committed) {
      //Console.println("******* log ********")
      //log.foreach(Console.println)
      committed = true
    }
}

