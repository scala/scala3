trait Lens[S, T] {
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
    def setterBody(obj: Term, value: Term, parts: List[String]): Term = {
      // o.copy(field = value)
      def helper(obj: Term, value: Term, field: String): Term =
        Term.Select.overloaded(obj, "copy", Nil, Term.NamedArg(field, value) :: Nil)

      parts match {
        case field :: Nil => helper(obj, value, field)
        case field :: parts =>
          helper(obj, setterBody(Term.Select.unique(obj, field), value, parts), field)
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
      case Function(param :: Nil, Path(o, parts)) if o.symbol == param.symbol =>
        '{
          val setter = (t: T) => (s: S) => ~setterBody(('(s)).unseal, ('(t)).unseal, parts).seal[S]
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

trait Iso[S, A] {
  def from(a: A): S
  def to(s: S): A
}

object Iso {
  def apply[S, A](_from: A => S)(_to: S => A): Iso[S, A] = new Iso {
    def from(a: A): S = _from(a)
    def to(s: S): A = _to(s)
  }

  def impl[S: Type, A: Type](implicit refl: Reflection): Expr[Iso[S, A]] = {
    import refl._
    import util._
    import quoted.Toolbox.Default._

    val tpS = typeOf[S]
    val tpA = typeOf[A]

    // 1. S must be a case class
    // 2. A must be a tuple
    // 3. The parameters of S must match A
    if (tpS.classSymbol.flatMap(cls => if (cls.flags.is(Flags.Case)) Some(true) else None).isEmpty)
      throw new QuoteError("Only support generation for case classes")

    val cls = tpS.classSymbol.get

    val companion = tpS match {
      case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
      case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
    }

    if (cls.caseFields.size != 1)
      throw new QuoteError("Use GenIso.fields for case classes more than one parameter")

    val fieldTp = tpS.memberType(cls.caseFields.head)
    if (!(fieldTp =:= tpA))
      throw new QuoteError(s"The type of case class field $fieldTp does not match $tpA")

    '{
      // (p: S) => p._1
      val to = (p: S) =>  ~{ Term.Select.unique(('(p)).unseal, "_1").seal[A] }
      // (p: A) => S(p)
      val from = (p: A) =>  ~{ Term.Select.overloaded(Term.Ident(companion), "apply", Nil, ('(p)).unseal :: Nil).seal[S] }
      apply(from)(to)
    }
  }

  def implUnit[S: Type](implicit refl: Reflection): Expr[Iso[S, Unit]] = {
    import refl._
    import util._
    import quoted.Toolbox.Default._

    val tpS = typeOf[S]

    if (tpS.isSingleton) {
      val ident = Term.Ident(tpS.asInstanceOf[TermRef]).seal[S]
      '{
        Iso[S, Unit](Function.const(~ident))(Function.const(()))
      }
    }
    else if (tpS.classSymbol.flatMap(cls => if (cls.flags.is(Flags.Case)) Some(true) else None).nonEmpty) {
      val cls = tpS.classSymbol.get

      if (cls.caseFields.size != 0)
        throw new QuoteError("Use GenIso.fields for case classes more than one parameter")

      val companion = tpS match {
        case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
        case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
      }

      val obj = Term.Select.overloaded(Term.Ident(companion), "apply", Nil, Nil).seal[S]

      '{
        Iso[S, Unit](Function.const(~obj))(Function.const(()))
      }
    }
    else {
      throw new QuoteError("Only support generation for case classes or singleton types")
    }
  }
}

object GenIso {
  /**
   *   GenIso[Person, String]  ~~>
   *
   *   Iso[Person, String]
   *     { p => p._1 }
   *     { p => Person(p) }
   */
  inline def apply[S, A]: Iso[S, A] = ~Iso.impl[S, A]

  inline def fields[S, A]: Iso[S, A] = ???
  inline def unit[S]: Iso[S, Unit] = ~Iso.implUnit[S]
}

trait Prism[S, A] {
  def getOption(s: S): Option[A]
  def apply(a: A): S
}

object Prism {
  def apply[S, A](getOpt: S => Option[A])(app: A => S): Prism[S, A] = new Prism {
    def getOption(s: S): Option[A] = getOpt(s)
    def apply(a: A): S = app(a)
  }

  def impl[S: Type, A: Type](implicit refl: Reflection): Expr[Prism[S, A]] = ???
}

object GenPrism {
  /**
   *   GenPrism[Json, JStr]  ~~>
   *
   *   Prism[Json, JStr]{
   *     case JStr(v) => Some(v)
   *      case _      => None
   *   }(jstr => jstr)
   */
  inline def apply[S, A]: Prism[S, A] = ~Prism.impl[S, A]
}