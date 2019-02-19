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


    // obj.copy(a = obj.a.copy(b = a.b.copy(c = v)))
    def setterBody(obj: Term, value: Term, fields: List[String]): Term = {
      // o.copy(field = value)
      def helper(obj: Term, value: Term, field: String): Term =
        Term.Select.overloaded(obj, "copy", Nil, Term.NamedArg(field, value) :: Nil)

      fields match {
        case field :: Nil => helper(obj, value, field)
        case field :: fields =>
          helper(obj, setterBody(Term.Select.unique(obj, field), value, fields), field)
      }
    }

    object Path {
      private def recur(tree: Term, selects: List[String]): Option[(Term, List[String])] = tree match {
        case Term.Ident(_) if selects.nonEmpty => Some((tree, selects))
        case Term.Select(qual, name) => recur(qual, name :: selects)
        case _ => None
      }

      def unapply(t: Term): Option[(Term, List[String])] = recur(t, Nil)
    }

    object Function {
      def unapply(t: Term): Option[(List[ValDef], Term)] = t match {
        case Term.Inlined(
          None, Nil,
          Term.Block(
            (ddef @ DefDef(_, Nil, params :: Nil, _, Some(body))) :: Nil,
            Term.Lambda(meth, _)
          )
        ) if meth.symbol == ddef.symbol => Some((params, body))
        case _ => None
      }
    }

    // exception: getter.unseal.underlyingArgument
    getter.unseal match {
      case Function(param :: Nil, Path(o, fields)) if o.symbol == param.symbol =>
        '{
          val setter = (t: T) => (s: S) => ~setterBody(('(s)).unseal, ('(t)).unseal, fields).seal[S]
          apply(~getter)(setter)
        }
      case _ =>
        throw new QuoteError("Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`")
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
    inline def apply[T](get: => (S => T)): Lens[S, T] = ~Lens.impl('(get))
  }
}