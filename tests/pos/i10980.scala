class A:
  val x: AnyRef = Object()
  val y: AnyRef = Object()

@main def m: Unit =
  val a = new A
  val b: a.x.type = a.x
  val c: A { val x: a.x.type } = a
  val d: A { val x: a.x.type;  val y: a.y.type } = a
