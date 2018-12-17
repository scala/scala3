// package scala.util.control
object TailCalls {

  abstract class TailRec[+A] {

    final def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      this match {
        case Done(a) => Call(() => f(a))
        case c@Call(_) => Cont(c, f)
        case c: Cont[a1, b1] => Cont(c.a, (x: a1) => c.f(x) flatMap f)
      }

    @annotation.tailrec final def resume: Either[() => TailRec[A], A] = this match {
      case Done(a) => Right(a)
      case Call(k) => Left(k)
      case Cont(a, f) => a match {
        case Done(v) => f(v).resume
        case Call(k) => Left(() => k().flatMap(f))
        case Cont(b, g) => b.flatMap(x => g(x) flatMap f).resume
      }
    }

    @annotation.tailrec final def result: A = this match {
      case Done(a) => a
      case Call(t) => t().result
      case Cont(a, f) => a match {
        case Done(v) => f(v).result
        case Call(t) => t().flatMap(f).result
        case Cont(b, g) => b.flatMap(x => g(x) flatMap f).result
      }
    }
  }

  protected case class Call[A](rest: () => TailRec[A]) extends TailRec[A]

  protected case class Done[A](value: A) extends TailRec[A]

  protected case class Cont[A, B](a: TailRec[A], f: A => TailRec[B]) extends TailRec[B]
}
