class A(l: Any):
  def f = l

class B(l: => Any):
  def f = l

trait C(l: Any):
  def f = l

trait D(l: => Int):
  def f = l

object Test extends App:
  var x = 0
  //object c extends C(x)
  object d extends D({ x += 1; println("called"); x })
  assert(d.f + d.f == 3, x)


/*

trait D(l: => Int):
  def f = l

@main def Test =
  var x = 0
  val d = new D( { x += 1; x }){}
  assert(d.f + d.f == 3)*/



