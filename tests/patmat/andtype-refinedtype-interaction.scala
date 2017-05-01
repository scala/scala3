sealed trait S
trait Trait extends S
class Clazz extends S
object Obj extends S

trait T1
trait T2

class C1
class C2
class C3
class C4

class SubC1 extends C1
class SubSubC1 extends SubC1
class SubC2 extends C2
class SubSubC2 extends SubC2

trait Unrelated

object Test {
  /*
  Note: error message on this suggests Obj to be unmatched as well.
  This isn't a bug in atomic type intersection, but a consequence
  of approximating all types to be a subtype of a structural type.

  def basic1(s: S & { val x: Int }) = s match {
    case _: Unrelated => ;
  }
  */

  def basic2(s: S & C1 { val x: Int }) = s match {
    case _: Unrelated => ;
  }

  def doubleInheritance1(s: S & C1 { val x: Int } & C2) = s match {
    case _: Unrelated => ;
  }

  def doubleInheritance2(s: S & C1 { val x: Int }
                              & C2 { val x: Int }) =
    s match {
      case _: Unrelated => ;
    }

  def classOrTrait(s: S & (C1 | (C2 | T1)) { val x: Int }
                        & (C3 | (C4 | T2)) { val x: Int }) =
    s match {
      case _: Unrelated => ;
    }

  def traitOnly(s: S & (C1 | (C2 | T1)) { val x: Int }
                     & C3 { val x: Int }) =
    s match {
      case _: Unrelated => ;
    }

  def nestedDoubleInheritance(s: S & (C1 & C2) { val x: Int }) =
    s match {
      case _: Unrelated => ;
    }

  def subclassingA(s: S & (C1 | C2) { val x: Int }
                        & (C3 | SubC1) { val x: Int }) =
    s match {
      case _: Unrelated => ;
    }

  def subclassingB(s: S & (T1 & (C1 | SubC2)) { val x: Int }
                        & (T2 & (C2 | C3 | SubC1)) { val x: Int }
                        & SubSubC1 { val x: Int }) =
    s match {
      case _: Unrelated => ;
    }

  def subclassingC(s: S & (T1 & (C1 | SubC2)) { val x: Int }
                        & (T2 & (C2 | C3 | SubC1)) { val x: Int }
                        & SubSubC2 { val x: Int }) =
    s match {
      case _: Unrelated => ;
    }
}
