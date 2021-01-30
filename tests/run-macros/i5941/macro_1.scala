trait Lens[S, T] {
  def get(s: S): T
  def set(t: T, s: S) :S
}

import scala.quoted.*

object Lens {
  def apply[S, T](_get: S => T)(_set: T => S => S): Lens[S, T] = new Lens {
    def get(s: S): T = _get(s)
    def set(t: T, s: S): S = _set(t)(s)
  }

  def impl[S: Type, T: Type](getter: Expr[S => T])(using Quotes) : Expr[Lens[S, T]] = {
    import quotes.reflect.*
    import util.*

    // obj.copy(a = obj.a.copy(b = a.b.copy(c = v)))
    def setterBody(obj: Term, value: Term, parts: List[String]): Term = {
      // o.copy(field = value)
      def helper(obj: Term, value: Term, field: String): Term =
        Select.overloaded(obj, "copy", Nil, NamedArg(field, value) :: Nil)

      parts match {
        case field :: Nil => helper(obj, value, field)
        case field :: parts =>
          helper(obj, setterBody(Select.unique(obj, field), value, parts), field)
      }
    }

    object Path {
      private def recur(tree: Term, selects: List[String]): Option[(Term, List[String])] = tree match {
        case Ident(_) if selects.nonEmpty => Some((tree, selects))
        case Select(qual, name) => recur(qual, name :: selects)
        case _ => None
      }

      def unapply(t: Term): Option[(Term, List[String])] = recur(t, Nil)
    }

    object Function {
      def unapply(t: Term): Option[(List[ValDef], Term)] = t match {
        case Inlined(None, Nil, Lambda(params, body)) => Some((params, body))
        case _ => None
      }
    }

    // exception: getter.asTerm.underlyingArgument
    getter.asTerm match {
      case Function(param :: Nil, Path(o, parts)) if o.symbol == param.symbol =>
        '{
          val setter = (t: T) => (s: S) => ${ setterBody('s.asTerm, 't.asTerm, parts).asExprOf[S] }
          apply($getter)(setter)
        }
      case _ =>
        report.error("Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`")
        '{???}
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
    inline def apply[T](inline get: (S => T)): Lens[S, T] = ${ Lens.impl('get) }
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

  def impl[S: Type, A: Type](using Quotes) : Expr[Iso[S, A]] = {
    import quotes.reflect.*
    import util.*

    val tpS = TypeRepr.of[S]
    val tpA = TypeRepr.of[A]

    // 1. S must be a case class
    // 2. A must be a tuple
    // 3. The parameters of S must match A
    if (tpS.classSymbol.flatMap(cls => if (cls.flags.is(Flags.Case)) Some(true) else None).isEmpty) {
      report.error("Only support generation for case classes")
      return '{???}
    }

    val cls = tpS.classSymbol.get

    val companion = tpS match {
      case TypeRef(prefix, name) => TermRef(prefix, name)
    }

    if (cls.caseFields.size != 1) {
      report.error("Use GenIso.fields for case classes more than one parameter")
      return '{???}
    }

    val fieldTp = tpS.memberType(cls.caseFields.head)
    if (!(fieldTp =:= tpA)) {
      report.error(s"The type of case class field $fieldTp does not match $tpA")
      '{???}
    } else '{
      // (p: S) => p._1
      val to = (p: S) =>  ${ Select.unique('p.asTerm, "_1").asExprOf[A] }
      // (p: A) => S(p)
      val from = (p: A) =>  ${ Select.overloaded(Ident(companion), "apply", Nil, 'p.asTerm :: Nil).asExprOf[S] }
      apply(from)(to)
    }
  }

  def implUnit[S: Type](using Quotes) : Expr[Iso[S, 1]] = {
    import quotes.reflect.*
    import util.*

    val tpS = TypeRepr.of[S]

    if (tpS.isSingleton) {
      val ident = Ident(tpS.asInstanceOf[TermRef]).asExprOf[S]
      '{
        Iso[S, 1](Function.const($ident))(Function.const(1))
      }
    }
    else if (tpS.classSymbol.flatMap(cls => if (cls.flags.is(Flags.Case)) Some(true) else None).nonEmpty) {
      val cls = tpS.classSymbol.get

      if (cls.caseFields.size != 0) {
        report.error("Use GenIso.fields for case classes more than one parameter")
        return '{???}
      }

      val companion = tpS match {
        case TypeRef(prefix, name) => TermRef(prefix, name)
      }

      val obj = Select.overloaded(Ident(companion), "apply", Nil, Nil).asExprOf[S]

      '{
        Iso[S, 1](Function.const($obj))(Function.const(1))
      }
    }
    else {
      report.error("Only support generation for case classes or singleton types")
      '{???}
    }
  }

  // TODO: require whitebox macro
  def implFields[S: Type](using Quotes) : Expr[Iso[S, Any]] = ???
}

object GenIso {
  /**
   *   GenIso[Person, String]  ~~>
   *
   *   Iso[Person, String]
   *     { p => p._1 }
   *     { p => Person(p) }
   */
  inline def apply[S, A]: Iso[S, A] = ${ Iso.impl[S, A] }

  // TODO: require whitebox macro
  inline def fields[S]: Iso[S, Any] = ${ Iso.implFields[S] }

  inline def unit[S]: Iso[S, 1] = ${ Iso.implUnit[S] }
}

trait Prism[S, A] { outer =>
  def getOption(s: S): Option[A]
  def apply(a: A): S

  def composeIso[B](iso: Iso[A, B]): Prism[S, B] = new Prism {
    def getOption(s: S): Option[B] = outer.getOption(s).map(a => iso.to(a))
    def apply(b: B): S = outer(iso.from(b))
  }
}

object Prism {
  def apply[S, A](getOpt: S => Option[A])(app: A => S): Prism[S, A] = new Prism {
    def getOption(s: S): Option[A] = getOpt(s)
    def apply(a: A): S = app(a)
  }

  def impl[S: Type, A <: S : Type](using Quotes) : Expr[Prism[S, A]] = {
    import quotes.reflect.*
    import util.*

    '{
      val get = (p: S) =>  if (p.isInstanceOf[A]) Some(p.asInstanceOf[A]) else None
      val app = (p: A) =>  p
      apply(get)(app)
    }
  }
}

object GenPrism {
  /**
   *   GenPrism[Json, JStr]  ~~>
   *
   *   Prism[Json, JStr]{
   *     case JStr(v) => Some(v)
   *     case _       => None
   *   }(jstr => jstr)
   */
  inline def apply[S, A <: S]: Prism[S, A] = ${ Prism.impl[S, A] }
}
