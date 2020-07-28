object Test {
  // First example with a concrete type <: AnyVal
  def main(args: Array[String]): Unit = {
    val coll = new java.util.ArrayList[Int]()
    java.util.Collections.addAll(coll, 5, 6)
    println(coll.size())

    foo(5, 6)
  }

  // Second example with an abstract type not known to be <: AnyRef
  def foo[A](a1: A, a2: A): Unit = {
    val coll = new java.util.ArrayList[A]()
    java.util.Collections.addAll(coll, a1, a2)
    println(coll.size())
  }
}
