class A:
  def greeting(name: String = "you") = s"Hello $name"

class B:
  val a = A()
  export a.*

@main def Test =
  val b = B()

  println(b.a.greeting())      // works
  println(b.greeting("John"))  // works
  println(b.greeting())        // nope !