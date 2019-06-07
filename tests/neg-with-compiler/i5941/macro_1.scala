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
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(this.getClass.getClassLoader)
    import refl._
    import util._
    // obj.copy(field = value)
    def setterBody(obj: Expr[S], value: Expr[T], field: String): Expr[S] =
      Select.overloaded(obj.unseal, "copy", Nil, NamedArg(field, value.unseal) :: Nil).seal.cast[S]

    // exception: getter.unseal.underlyingArgument
    getter.unseal match {
      case Inlined(
        None, Nil,
        Block(
          DefDef(_, Nil, (param :: Nil) :: Nil, _, Some(Select(o, field))) :: Nil,
          Lambda(meth, _)
        )
      ) if o.symbol == param.symbol =>
        '{
          val setter = (t: T) => (s: S) => ${ setterBody('s, 't, field) }
          apply($getter)(setter)
        }
      case _ =>
        QuoteError("Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`")
    }
  }
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