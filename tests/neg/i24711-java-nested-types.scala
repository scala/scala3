class Test {
  val test1: Int = (??? : java.util.Map.Entry[String, Int])  // error

  val test2: Int = java.util.Map.entry("key", 1)  // error

  trait Outer { type Inner }
  val test4: Int = (??? : Outer#Inner)  // error
}
