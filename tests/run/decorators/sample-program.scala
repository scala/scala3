object myProgram:

  /** Adds two numbers
   *  @param  num   the first number
   *  @param  inc   the second number
   */
  @main def add(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

  /** @usage java addAll --nums <numbers> */
  @join @logged @split @main def addAll(nums: Int*): Unit =
    println(nums.sum)

end myProgram

//  Compiler generated code:

object add:
  private val $main = new main()
  private val $main$wrapper = $main.wrapper(
    "MyProgram.add",
    """Adds two numbers
      |@param  num   the first number
      |@param  inc   the second number""".stripMargin)
  def main(args: Array[String]): Unit =
    val cll = $main$wrapper.call(args)
    val arg1 = cll.nextArgGetter[Int]("num", summon[$main.ArgumentParser[Int]])
    val arg2 = cll.nextArgGetter[Int]("inc", summon[$main.ArgumentParser[Int]], Some(1))
    cll.run(myProgram.add(arg1(), arg2()))
end add

object addAll:
  private val $main = new main()
  private val $split = new split()
  private val $logged = new logged()
  private val $join = new join()
  private val $main$wrapper = $main.wrapper("MyProgram.addAll", "@usage java addAll --nums <numbers>")
  private val $split$wrapper = $split.wrapper($main$wrapper)
  private val $logged$wrapper = $logged.wrapper($split$wrapper)
  private val $join$wrapper = $join.wrapper($logged$wrapper)
  def main(args: Array[String]): Unit =
      $join$wrapper.adapt { (args: String) =>
        $logged$wrapper.adapt { (args: String) =>
          $split$wrapper.adapt { (args: Array[String]) =>
          val cll = $main$wrapper.call(args)
          val arg1 = cll.finalArgsGetter[Int]("nums", summon[$main.ArgumentParser[Int]])
          cll.run(myProgram.addAll(arg1()*))
          } (args)
        } (args)
      } (args)
end addAll