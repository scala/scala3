object Test {
  def main(args:Array[String]): Unit = println(foo(1, 2, 3)())
  def foo(args: Int*)(arg: String = "foo"): String = arg + args.mkString("")
}
