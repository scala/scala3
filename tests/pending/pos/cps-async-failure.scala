
import scala.quoted.*

trait App[F[_],CT]:
  this: Base[F,CT] =>

  import quotes.reflect.*

  trait AA

  def f(cpsCtx: FC[F]): AA =
    g(cpsCtx)

  def g(cpsCtx: FC[F]): AA =
    val paramSym: Symbol = ???
    println(paramSym.tree)   // println necessary for failure
    val t: Term = ???
    t match {                // match necessary for failure
      case Typed(_, _) => ???  // both cases necessary for failure
      case Lambda(_, _) => ???
    }
    f(cpsCtx)                 // necessary for failure
    val cpsBody = runRoot()   // necessary for failure
    g(cpsCtx)                 // failure
      // 26 |    g(cpsCtx)
      //    |    ^
      //    |Ambiguous overload. The overloaded alternatives of method g in trait App with types
      //    | (cpsCtx: FC[F]): Base.this.AA
      //    | (cpsCtx: FC[F]): App.this.AA
      //    |both match arguments ((cpsCtx : FC[F]))
      //    |
      //    |Note: this happens because two or more alternatives have the same erasure,
      //    |      so they cannot be distinguished by overloading resolution


class FC[F[_]]()

trait Base[F[_]:Type,CT:Type]  // Both :Type context bounds are necessary for failure
extends Cps with Root[F, CT] with App[F, CT]:
  implicit val qctx: Quotes

trait Root[F[_], CT]:
  this: Base[F, CT] =>
  def runRoot(): CpsTree = ???

trait Cps:
  sealed abstract class CpsTree

