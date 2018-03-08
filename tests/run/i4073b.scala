object Test {
  def main(args:Array[String]): Unit = println(foo(1, 2, 3)())
  def foo(args: Any*)(arg: String = "foo"): String = arg + args.mkString("")
}
