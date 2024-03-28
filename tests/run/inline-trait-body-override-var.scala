inline trait A:
  var x: Int

class B extends A:
  var x: Int = 2

@main def Test =
  val b = B()
  println(b.x)
