// (x: Any) match {
//   case _: T         => // x.iIO[T]
//   case _: T | _: U  => // x.iIO[T] || x.iIO[U]
//   // case _: p.type    => // x eq p
//   case _: T[A1, A2] => // x.iIO[T[_, _]]
//   case 1            => // 1 == x // Overloading?
//   case Nil          => // Nil == x

//   case UnapplyBooleanT(_)   => // x.iIO[T] && UnapplyBooleanT.unapply(x.aIO)
//   case UnapplyProductT(_)   => // x.iIO[T]
//   // case UnapplyNameBasedT(_) => // x.iIO[T] && !UnapplyNameBasedT.unapply(x.aIO).isEmpty
// }

trait F[X]
trait T
trait U
object NIL

object UnapplyBooleanT {
  def unapply(t: T): Boolean = ???
}
object UnapplyBooleanT_true {
  dependent def unapply(t: T): Boolean = true
}
object UnapplyBooleanT_false {
  dependent def unapply(t: T): Boolean = false
}

object UnapplyProductT {
  def unapply(t: T): Tuple2[Int, String] = (1, "")
}

object UnapplyNameBasedT {
  def unapply(t: T): Option[String] = ???
}
object UnapplyNameBasedT_None {
  dependent def unapply(t: T): Option[String] = None
}
object UnapplyNameBasedT_Some {
  dependent def unapply(t: T): Option[String] = Some("")
}

object Test {
  var any: Any = null

  // --------------------------------------------------------------------------
   dependent def typePattern(x: Any) =
     x match {
       case _: T => 1
       case _    => 2
     }
  dependent def typeDesugared(x: Any) =
    if (x.isInstanceOf[T]) 1 else 2

  // typePattern(any): { typeDesugared(any) } //-
  // typePattern(new T{})   : 1 //-
  val t: T = new T{}
  typeDesugared(t) : 1 //-
  // typePattern("")        : 2 //-
  typeDesugared("")      : 2 //-

  // --------------------------------------------------------------------------
   dependent def typedHKPattern(x: Any) =
     x match {
       case _: F[Int] => 1
       case _         => 2
     }
  dependent def typedHKDesugared(x: Any) =
    if (x.isInstanceOf[F[Int]]) 1 else 2

  // typedHKPattern(any): { typedHKDesugared(any) } //-
//   typedHKPattern(new F[Int]{})     : 1 //-
  typedHKDesugared(new F[String]{}) : 1 //-
//   typedHKPattern("")               : 2 //-
  typedHKDesugared("")              : 2 //-

  // --------------------------------------------------------------------------
   dependent def alternativePattern(x: Any) =
     x match {
       case _: T | _: U => 1
       case _           => 2
     }
  dependent def alternativeDesugared(x: Any) =
    if (x.isInstanceOf[T] || x.isInstanceOf[U]) 1 else 2

  // alternativePattern(any): { alternativeDesugared(any) } //-
  // alternativePattern(new T{})          : 1 //-
  alternativeDesugared(new T{}) : 1 //-
  // alternativePattern(new U{})          : 1 //-
  alternativeDesugared(new U{}) : 1 //-
  // alternativePattern("")               : 2 //-
  alternativeDesugared("")      : 2 //-

  // --------------------------------------------------------------------------
   dependent def stablePattern(x: Any) =
     x match {
       case NIL => 1
       case _   => 2
     }
  dependent def stableDesugared(x: Any) =
    if (NIL == x) 1 else 2

  // stablePattern(any): { stableDesugared(any) } //-
  // // For these cases we would need to dependentify AnyRef.== (to `eq`) //-
  // // and prove that there is only one value of `NIL`. //-
  // stablePattern(NIL)          : 1 //-
  // stableDesugared(NIL) : 1 //-
  // stablePattern("")           : 2 //-
  // stableDesugared("")  : 2 //-

  // --------------------------------------------------------------------------
   dependent def literalPattern(x: Any) =
     x match {
       case 0 => 1
       case _ => 2
     }
  dependent def literalDesugared(x: Any) =
    if (0 == x) 1 else 2

  // literalPattern(any): { literalDesugared(any) } //-
  // literalPattern(0)           : 1 //-
  literalDesugared(0)  : 1 //-
  // These two get stuck on `0 == ""`, which is not simplifed to false by the constant folder...
  // literalPattern("")   : 2 //-
  // literalDesugared("") : 2 //-

  // --------------------------------------------------------------------------
   dependent def unapplyBoolPattern(x: Any) =
     x match {
       case UnapplyBooleanT() => 1
       case _                  => 2
     }
   dependent def unapplyTruePattern(x: Any) =
     x match {
       case UnapplyBooleanT_true() => 1
       case _                       => 2
     }
   dependent def unapplyFalsePattern(x: Any) =
     x match {
       case UnapplyBooleanT_false() => 1
       case _                        => 2
     }

  dependent def unapplyBoolDesugared(x: Any) =
    if (x.isInstanceOf[T] && UnapplyBooleanT.unapply(x.asInstanceOf[T])) 1 else 2

  dependent def unapplyTrueDesugared(x: Any) =
    if (x.isInstanceOf[T] && UnapplyBooleanT_true.unapply(x.asInstanceOf[T])) 1 else 2

  dependent def unapplyFalseDesugared(x: Any) =
    if (x.isInstanceOf[T] && UnapplyBooleanT_false.unapply(x.asInstanceOf[T])) 1 else 2

  // unapplyBoolPattern(any): { unapplyBoolDesugared(any) } //-
  // unapplyTruePattern(any): { unapplyTrueDesugared(any) } //-
  // unapplyFalsePattern(any): { unapplyFalseDesugared(any) } //-

  // unapplyBoolPattern("")         : 2 //-
  // unapplyTruePattern("")         : 2 //-
  // unapplyFalsePattern("")        : 2 //-
  // These 3 cases require the normalizer to implement the short circuit semantic of &&:
  // unapplyBoolDesugared("")       : 2 //-
  // unapplyTrueDesugared("")       : 2 //-
  // unapplyFalseDesugared("")      : 2 //-

  // unapplyTruePattern(new T{})    : 1 //-
  unapplyTrueDesugared(new T{})  : 1 //-
  // unapplyFalsePattern(new T{})   : 2 //-
  unapplyFalseDesugared(new T{}) : 2 //-

   dependent def unapplyProductPattern(x: Any) =
     x match {
       case UnapplyProductT(_, _) => 1
       case _                     => 2
     }
  dependent def unapplyProductDesugared(x: Any) =
    if (x.isInstanceOf[T]) 1 else 2

  // unapplyProductPattern(any): { unapplyProductDesugared(any) } //-
  // unapplyProductPattern(new T{})   : 1 //-
  unapplyProductDesugared(new T{}) : 1 //-
  // unapplyProductPattern("")        : 2 //-
  unapplyProductDesugared("")      : 2 //-

  // TODO
  // case UnapplyNameBasedT(_) => 1
  // case UnapplyNameBasedT_None(_) => 1
  // case UnapplyNameBasedT_Some(_) => 1
}
