import dotty.tools.dotc.core.Types._

object Patterns {
  val d: Object = null
  private def rebase(tp: NamedType): Type = {
    def rebaseFrom(prefix: Type): Type = ???
    tp.prefix match {
      case RefinedThis(rt) => rebaseFrom(rt)
      case pre: ThisType => rebaseFrom(pre)
      case _ => tp
    }
  }
  d match {
    case WildcardType(bounds: TypeBounds) =>
      bounds.variance
  }

  ('1', "1") match {
    case (digit, str) => true
    case _ => false
  }


  def foo2(x: AnyRef) = x match { case x: Function0[Any] => x() }
  object Breakdown {
    def unapplySeq(x: Int): Some[List[String]] = Some(List("", "there"))
  }

  object Test2 {
    42 match {
      case a@Breakdown(f@"") =>  // needed to trigger bug
      case b@Breakdown(d@"foo") =>  // needed to trigger bug
      case c@Breakdown(e@"", who) => println ("hello " + who)
    }
  }

  val names = List("a", "b", "c")
  object SeqExtractors {
    val y = names match {
      case List(x, z) => x
      case List(x) => x
      case List() => ""
    }
    val yy: String = y
  }



  val xs = List('2' -> "ABC", '3' -> "DEF")

  xs filter {
    case (digit, str) => true
    case _ => false
  }

  (xs: Any) match {
    case x: Int @unchecked => true
    case xs: List[Int @ unchecked] => true
    case _ => false
  }

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case x :: xs1 => x + sum(xs1)
  }

  def len[T](xs: List[T]): Int = xs match {
    case _ :: xs1 => 1 + len(xs1)
    case Nil => 0
  }

  final def sameLength[T](xs: List[T], ys: List[T]): Boolean = xs match {
    case _ :: xs1 => xs1.isEmpty
      ys match {
        case _ :: ys1 => sameLength(xs1, ys1)
        case _ => false
      }
    case _ => ys.isEmpty
  }
}
