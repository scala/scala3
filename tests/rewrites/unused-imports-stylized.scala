// scalac: -Wunused:imports -Werror -feature -rewrite -Yrewrite-imports

import language.{ future,
                  implicitConversions,
                  postfixOps,
                }
import scala.concurrent.*, ExecutionContext.Implicits.*

class C:
  def c = 42
  import collection.mutable, mutable.ListBuffer, mutable.{HashMap as GoodMap, Seq as _, Buffer, Set as OK}

  def buf = ListBuffer.empty[String]
  def ok: OK[Int] = ???
  def f = concurrent.Future(42)  // implicit usage

  import Thread.*
  import State.{
    NEW,
    BLOCKED}

  def state(t: Thread) =
    t.getState match
      case NEW =>
        import State.RUNNABLE
        t.getState match
          case RUNNABLE => 0
          case BLOCKED => 1
          case _ => -1
      case _ => -1

  enum E:
    case E0, E1

  def e(x: E) =
    x match
      case E.E0 => "e0"
      case E.E1 => "e1"

  locally {
    import Givens.{
      *,
      given
    }

    def g(s: String)(using Ordering[C]) = ???
    def ordered = g(greeting)
  }
  locally {
    import Givens.{
      cOrdering,
      *}

    def g(s: String)(using Ordering[C]) = ???
    def ordered = g(greeting)
  }
  locally {
    import Givens
           .{given Ordering[C], *},
           E
           .{given, *}

    def g(s: String)(using Ordering[C]) = ???
    println(g(""))
  }
  locally {
    import
      E.{given, *},
      Givens.{given Ordering[C], *}

    def g(s: String)(using Ordering[C]) = ???
    println(g(""))
  }
end C

object Givens:
  given cOrdering: Ordering[C] with
    override def compare(c0: C, c1: C) = 0
  val greeting = "we love Givens"

class A:
  def f(b: B): Unit = b.f(this)

object A:
  implicit val a2b: Conversion[A, B] = _ => B()

class B:
  def f(b: B): Unit = ()
