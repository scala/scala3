trait Api:
  def alpha: Int
  def bravo: Int
  def charlie: Int
  def delta: Int
  def echo: Int
  def foxtrot: Int

class Fields:
  val alpha, bravo, charlie, delta, echo, foxtrot = 0

object Test:
  def main(args: Array[String]): Unit =
    println(s"methodMembers: ${memberNames[Api]._1}")
    println(s"fieldMembers:  ${memberNames[Fields]._2}")
