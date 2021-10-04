// Sample main method
object myProgram:

  /** Checks that the correct amount of parameters were passed */
  @main def count(count: Int, elems: String*): Unit =
    if (elems.length == count)
      println("Correct")
    else
      println(s"Expected $count argument${if (count != 1) "s" else ""}, but got ${elems.length}")
      println(s"  ${elems.mkString(", ")}")

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object count extends main:
  def main(args: Array[String]) =
    val cmd = command(args)
    val arg1 = cmd.argGetter[Int]("count", summon[ArgumentParser[Int]])
    val arg2 = cmd.argsGetter[String]("elems", summon[ArgumentParser[String]])

    cmd.run(myProgram.count(arg1(), arg2(): _*), "count", "Checks that the correct amount of parameters were passed")
end count

object Test:
  def main(args: Array[String]): Unit =
    count.main(Array("1", "Hello"))
    count.main(Array("2", "Hello", "world!"))
    count.main(Array("3", "No 3 elements"))
    count.main(Array("0"))
    count.main(Array("0", "I", "shouldn't", "be", "here"))
    count.main(Array("-2", "How does that make sense?"))
    count.main(Array("26") ++ ('a' to 'z').toArray.map(_.toString))
end Test
