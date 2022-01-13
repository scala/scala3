class A:
  type X
  val x: Int = 1
  def foo: Int = 1
@main def Test: Unit =
  println(allMembers[Object])
  println(allMembers[A])