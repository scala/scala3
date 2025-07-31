
final class Implicit()

final class Id[+A, -U](val value: A):
  def map[B](f: A => B)(using Implicit): Id[B, U] = ??? //Id(f(value))
  def flatMap[B, V <: U](f: A => Id[B, V]): Id[B, V] = f(value)
  def run: A = value

type Foo = Foo.type
case object Foo:
  def get: Id[Int, Foo] = Id(42)

type Bar = Bar.type
case object Bar:
  def inc(i: Int): Id[Int, Bar] = Id(i * 10)

def program(using Implicit) =
  for
    a <- Foo.get
    x <- Bar.inc(a)
  yield x

@main def Test = println:
  given Implicit = Implicit()
  program.run
