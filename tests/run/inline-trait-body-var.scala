inline trait A:
  var x = 1

class B extends A:
  def f =
    val old = x
    x += 1
    old

@main def Test =
  val b = B()
  println(b.f)
  println(b.f)
  println(b.f)
  println(b.f)
