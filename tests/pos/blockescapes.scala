object blockescapes {

  { val x = 0; () }
  val x0 = { class Foo; new Foo }
  val x1 = {}
  var x2 = { val z = 0 }
  val m1 = { val x = 2; x }

  trait T
  def m0: T = { object Foo { class Bar extends T } ; new Foo.Bar }
}
