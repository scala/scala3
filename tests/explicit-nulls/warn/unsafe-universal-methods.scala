//> using options -deprecation

class CustomAnyRef // definition ok

class CustomAnyVal(x: Double) extends AnyVal // definition ok

case class CaseClass(x: Double, y: String) // definition ok

object Test {
  // --- hashCode -- concrete classes ---

  // must warn
  def hashCode1(x: Any): Int = x.hashCode() // warn
  def hashCode2(x: AnyVal): Int = x.hashCode() // warn
  def hashCode3(x: Matchable): Int = x.hashCode() // warn
  def hashCode4(x: Int | Null): Int = x.hashCode() // warn
  def hashCode5(x: List[String] | Null): Int = x.hashCode() // warn
  def hashCode6(x: Float): Int = x.hashCode() // warn
  def hashCode7(x: Double): Int = x.hashCode() // warn

  // these could become 'ok' one day; only Float and Double are problematic
  def hashCode8(x: Boolean): Int = x.hashCode() // warn
  def hashCode9(x: Int): Int = x.hashCode() // warn
  def hashCode10(x: Char): Int = x.hashCode() // warn

  // slightly undesirable, but no big deal; changing to 'ok' would be a progression
  def hashCode11(x: CustomAnyVal): Int = x.hashCode() // warn

  // must be ok
  def hashCodeOK1(x: AnyRef): Int = x.hashCode() // ok
  def hashCodeOK2(x: String): Int = x.hashCode() // ok
  def hashCodeOK3(x: CustomAnyRef): Int = x.hashCode() // ok
  def hashCodeOK4(x: CaseClass): Int = x.hashCode() // ok

  // --- hashCode -- type parameter bound ---

  // must warn
  def hashCodeGen1[T <: Any](x: T): Int = x.hashCode() // warn
  def hashCodeGen2[T <: AnyVal](x: T): Int = x.hashCode() // warn
  def hashCodeGen3[T <: Matchable](x: T): Int = x.hashCode() // warn
  def hashCodeGen4[T <: Int | Null](x: T): Int = x.hashCode() // warn
  def hashCodeGen5[T <: List[String] | Null](x: T): Int = x.hashCode() // warn
  def hashCodeGen6[T <: Float](x: T): Int = x.hashCode() // warn
  def hashCodeGen7[T <: Double](x: T): Int = x.hashCode() // warn

  // these could become 'ok' one day; only Float and Double are problematic
  def hashCodeGen8[T <: Boolean](x: T): Int = x.hashCode() // warn
  def hashCodeGen9[T <: Int](x: T): Int = x.hashCode() // warn
  def hashCodeGen10[T <: Char](x: T): Int = x.hashCode() // warn

  // slightly undesirable, but no big deal; changing to 'ok' would be a progression
  def hashCodeGen11[T <: CustomAnyVal](x: T): Int = x.hashCode() // warn

  // must be ok
  def hashCodeGenOK1[T <: AnyRef](x: T): Int = x.hashCode() // ok
  def hashCodeGenOK2[T <: String](x: T): Int = x.hashCode() // ok
  def hashCodeGenOK3[T <: CustomAnyRef](x: T): Int = x.hashCode() // ok
  def hashCodeGenOK4[T <: CaseClass](x: T): Int = x.hashCode() // ok

  // --- equals -- concrete classes ---

  // must warn
  def equals1(x: Any): Boolean = x.equals(42) // warn
  def equals2(x: AnyVal): Boolean = x.equals(42) // warn
  def equals3(x: Matchable): Boolean = x.equals(42) // warn
  def equals4(x: Int | Null): Boolean = x.equals(42) // warn
  def equals5(x: List[String] | Null): Boolean = x.equals(42) // warn
  def equals6(x: Float): Boolean = x.equals(42) // warn
  def equals7(x: Double): Boolean = x.equals(42) // warn

  // these could become 'ok' one day; only Float and Double are problematic
  def equals8(x: Boolean): Boolean = x.equals(42) // warn
  def equals9(x: Int): Boolean = x.equals(42) // warn
  def equals10(x: Char): Boolean = x.equals(42) // warn

  // slightly undesirable, but no big deal; changing to 'ok' would be a progression
  def equals11(x: CustomAnyVal): Boolean = x.equals(42) // warn

  // must be ok
  def equalsOK1(x: AnyRef): Boolean = x.equals(42) // ok
  def equalsOK2(x: String): Boolean = x.equals(42) // ok
  def equalsOK3(x: CustomAnyRef): Boolean = x.equals(42) // ok
  def equalsOK4(x: CaseClass): Boolean = x.equals(42) // ok

  // --- equals -- type parameter bound ---

  // must warn
  def equalsGen1[T <: Any](x: T): Boolean = x.equals(42) // warn
  def equalsGen2[T <: AnyVal](x: T): Boolean = x.equals(42) // warn
  def equalsGen3[T <: Matchable](x: T): Boolean = x.equals(42) // warn
  def equalsGen4[T <: Int | Null](x: T): Boolean = x.equals(42) // warn
  def equalsGen5[T <: List[String] | Null](x: T): Boolean = x.equals(42) // warn
  def equalsGen6[T <: Float](x: T): Boolean = x.equals(42) // warn
  def equalsGen7[T <: Double](x: T): Boolean = x.equals(42) // warn

  // these could become 'ok' one day; only Float and Double are problematic
  def equalsGen8[T <: Boolean](x: T): Boolean = x.equals(42) // warn
  def equalsGen9[T <: Int](x: T): Boolean = x.equals(42) // warn
  def equalsGen10[T <: Char](x: T): Boolean = x.equals(42) // warn

  // slightly undesirable, but no big deal; changing to 'ok' would be a progression
  def equalsGen11[T <: CustomAnyVal](x: T): Boolean = x.equals(42) // warn

  // must be ok
  def equalsGenOK1[T <: AnyRef](x: T): Boolean = x.equals(42) // ok
  def equalsGenOK2[T <: String](x: T): Boolean = x.equals(42) // ok
  def equalsGenOK3[T <: CustomAnyRef](x: T): Boolean = x.equals(42) // ok
  def equalsGenOK4[T <: CaseClass](x: T): Boolean = x.equals(42) // ok
}
