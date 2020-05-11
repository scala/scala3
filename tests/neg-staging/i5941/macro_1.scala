abstract class Lens[S, T] {
  def get(s: S): T
  def set(t: T, s: S) :S
}

import scala.quoted._

object Lens {
  def apply[S, T](_get: S => T)(_set: T => S => S): Lens[S, T] = new Lens {
    def get(s: S): T = _get(s)
    def set(t: T, s: S): S = _set(t)(s)
  }

  def impl[S, T](using s: Scope)(getter: s.Expr[S => T])(using s.Type[S], s.Type[T]): s.Expr[Lens[S, T]] = {
    import s.tasty._
    import util._

    // exception: getter.underlyingArgument
    getter match {
      case Inlined(
        None, Nil,
        Block(
          DefDef(_, Nil, (param :: Nil) :: Nil, _, Some(Select(o, field))) :: Nil,
          Lambda(meth, _)
        )
      ) if o.symbol == param.symbol =>
        '{
          val setter = (t: T) => (x: S) => ${ setterBody('x, 't, field) }
          apply($getter)(setter)
        }
      case _ =>
        report.error("Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`")
        '{???}
    }
  }

  // obj.copy(field = value)
  def setterBody[S, T](using s: Scope)(obj: s.Expr[S], value: s.Expr[T], field: String)(using s.Type[S]): s.Expr[S] =
    import s.tasty._
    Select.overloaded(obj, "copy", Nil, NamedArg(field, value) :: Nil).seal.cast[S]

}

object GenLens {
  /** case class Address(streetNumber: Int, streetName: String)
   *
   *  GenLens[Address](_.streetNumber)   ~~>
   *
   *  Lens[Address, Int](_.streetNumber)(n => a => a.copy(streetNumber = n))
   */

  def apply[S] = new MkGenLens[S]
  class MkGenLens[S] {
    inline def apply[T](get: => (S => T)): Lens[S, T] = ${ Lens.impl('get) }
  }
}