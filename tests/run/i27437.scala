
import util.chaining.*

case class Ctx(label: String)

case class M[A](a: A):
  var mapped, flatMapped = 0
  def flatMap[B](f: A => M[B])(using c: Ctx): M[B] =
    f(a).tap: m =>
      m.flatMapped += flatMapped + 1
  def map[B](f: A => B)(using c: Ctx): M[B] =
    M(f(a)).tap: m =>
      m.mapped += mapped + 1

def retain(): Unit =
  var dummies = 0
  given DummyImplicit => Ctx =
    Ctx("given").tap: _ =>
      dummies += 1
  val res =
    for
      n <- M(1)
      r <- M(n)
    yield r
  assert(dummies == 2)
  assert(res.mapped == 1)
  assert(res.flatMapped == 1)

def discard(): Unit =
  given Ctx("given")
  val res =
    for
      n <- M(1)
      p <- M(n)
      r <- M(p)
    yield r
  assert(res.mapped == 0)
  assert(res.flatMapped == 2)

@main def Test: Unit =
  retain()
  discard()
