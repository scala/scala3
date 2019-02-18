abstract class Lens[S, T] {
  def get(s: S): T
  def set(t: T, s: S) :S
}

import scala.quoted._
import scala.tasty._

object Lens {
  def apply[S, T](_get: S => T)(_set: T => S => S): Lens[S, T] = new Lens {
    def get(s: S): T = _get(s)
    def set(t: T, s: S): S = _set(t)(s)
  }

  def impl[S: Type, T: Type](getter: Expr[S => T])(implicit refl: Reflection): Expr[Lens[S, T]] = {
    import refl._
    import util._
    import quoted.Toolbox.Default._

    // obj.copy(field = value)
    def setterBody(obj: Expr[S], value: Expr[T], field: String): Expr[S] =
      Term.Select.overloaded(obj.unseal, "copy", Nil, Term.NamedArg(field, value.unseal) :: Nil).seal[S]

    getter.unseal.underlyingArgument match {
      case Term.Block(
        DefDef(_, Nil, (param :: Nil) :: Nil, _, Some(Term.Select(o, field))) :: Nil,
        Term.Lambda(meth, _)
      ) =>
        '{
          val setter = (t: T) => (s: S) => ~setterBody('(s), '(t), field)
          apply(~getter)(setter)
        }
    }
  }
}

object GenLens {
  /** case class Address(streetNumber: Int, streetName: String)
   *
   *  Lens.gen[Address, Int](_.streetNumber)   ~~>
   *
   *  Lens[Address, Int](_.streetNumber)(n => a => a.copy(streetNumber = n))
   */

  def apply[S] = new MkGenLens[S]
  class MkGenLens[S] {
    inline def apply[T](get: S => T): Lens[S, T] = ~Lens.impl('(get))
  }
}