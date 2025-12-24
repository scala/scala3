package scala.quoted.compiletime

/////// Constant ///////////////////////////////////////////////////////////////

sealed trait Constant {

  def value: Any

  final def show(using p: Printer[Constant]): String = p.show(this)

}
object Constant {

  def quoted(using quotes: Quotes): Constant.Module = quotes.reflectV2.Constant
  given moduleConversion: (quotes: Quotes) => Conversion[Constant.type, Constant.Module] = _ => quotes.reflectV2.Constant

  def unapply(x: Constant): Some[Any] = Some(x.value)

  trait Module private[compiletime] () {}

}

/////// BooleanConstant ///////////////////////////////////////////////////////////////

trait BooleanConstant private[compiletime] () extends Constant {

  override def value: Boolean

}
object BooleanConstant {

  def quoted(using quotes: Quotes): BooleanConstant.Module = quotes.reflectV2.BooleanConstant
  given moduleConversion: (quotes: Quotes) => Conversion[BooleanConstant.type, BooleanConstant.Module] = _ => quotes.reflectV2.BooleanConstant

  def unapply(x: BooleanConstant): Some[Boolean] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Boolean): BooleanConstant
    def make(x: Boolean): BooleanConstant
  }

}

/////// ByteConstant ///////////////////////////////////////////////////////////////

trait ByteConstant private[compiletime] () extends Constant {

  override def value: Byte

}
object ByteConstant {

  def quoted(using quotes: Quotes): ByteConstant.Module = quotes.reflectV2.ByteConstant
  given moduleConversion: (quotes: Quotes) => Conversion[ByteConstant.type, ByteConstant.Module] = _ => quotes.reflectV2.ByteConstant

  def unapply(x: ByteConstant): Some[Byte] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Byte): ByteConstant
    def make(x: Byte): ByteConstant
  }

}

/////// ShortConstant ///////////////////////////////////////////////////////////////

trait ShortConstant private[compiletime] () extends Constant {

  override def value: Short

}
object ShortConstant {

  def quoted(using quotes: Quotes): ShortConstant.Module = quotes.reflectV2.ShortConstant
  given moduleConversion: (quotes: Quotes) => Conversion[ShortConstant.type, ShortConstant.Module] = _ => quotes.reflectV2.ShortConstant

  def unapply(x: ShortConstant): Some[Short] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Short): ShortConstant
    def make(x: Short): ShortConstant
  }

}

/////// IntConstant ///////////////////////////////////////////////////////////////

trait IntConstant private[compiletime] () extends Constant {

  override def value: Int

}
object IntConstant {

  def quoted(using quotes: Quotes): IntConstant.Module = quotes.reflectV2.IntConstant
  given moduleConversion: (quotes: Quotes) => Conversion[IntConstant.type, IntConstant.Module] = _ => quotes.reflectV2.IntConstant

  def unapply(x: IntConstant): Some[Int] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Int): IntConstant
    def make(x: Int): IntConstant
  }

}

/////// LongConstant ///////////////////////////////////////////////////////////////

trait LongConstant private[compiletime] () extends Constant {

  override def value: Long

}
object LongConstant {

  def quoted(using quotes: Quotes): LongConstant.Module = quotes.reflectV2.LongConstant
  given moduleConversion: (quotes: Quotes) => Conversion[LongConstant.type, LongConstant.Module] = _ => quotes.reflectV2.LongConstant

  def unapply(x: LongConstant): Some[Long] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Long): LongConstant
    def make(x: Long): LongConstant
  }

}

/////// FloatConstant ///////////////////////////////////////////////////////////////

trait FloatConstant private[compiletime] () extends Constant {

  override def value: Float

}
object FloatConstant {

  def quoted(using quotes: Quotes): FloatConstant.Module = quotes.reflectV2.FloatConstant
  given moduleConversion: (quotes: Quotes) => Conversion[FloatConstant.type, FloatConstant.Module] = _ => quotes.reflectV2.FloatConstant

  def unapply(x: FloatConstant): Some[Float] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Float): FloatConstant
    def make(x: Float): FloatConstant
  }

}

/////// DoubleConstant ///////////////////////////////////////////////////////////////

trait DoubleConstant private[compiletime] () extends Constant {

  override def value: Double

}
object DoubleConstant {

  def quoted(using quotes: Quotes): DoubleConstant.Module = quotes.reflectV2.DoubleConstant
  given moduleConversion: (quotes: Quotes) => Conversion[DoubleConstant.type, DoubleConstant.Module] = _ => quotes.reflectV2.DoubleConstant

  def unapply(x: DoubleConstant): Some[Double] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Double): DoubleConstant
    def make(x: Double): DoubleConstant
  }

}

/////// CharConstant ///////////////////////////////////////////////////////////////

trait CharConstant private[compiletime] () extends Constant {

  override def value: Char

}
object CharConstant {

  def quoted(using quotes: Quotes): CharConstant.Module = quotes.reflectV2.CharConstant
  given moduleConversion: (quotes: Quotes) => Conversion[CharConstant.type, CharConstant.Module] = _ => quotes.reflectV2.CharConstant

  def unapply(x: CharConstant): Some[Char] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: Char): CharConstant
    def make(x: Char): CharConstant
  }

}

/////// StringConstant ///////////////////////////////////////////////////////////////

trait StringConstant private[compiletime] () extends Constant {

  override def value: String

}
object StringConstant {

  def quoted(using quotes: Quotes): StringConstant.Module = quotes.reflectV2.StringConstant
  given moduleConversion: (quotes: Quotes) => Conversion[StringConstant.type, StringConstant.Module] = _ => quotes.reflectV2.StringConstant

  def unapply(x: StringConstant): Some[String] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: String): StringConstant
    def make(x: String): StringConstant
  }

}

/////// UnitConstant ///////////////////////////////////////////////////////////////

trait UnitConstant private[compiletime] () extends Constant {

  override def value: Unit

}
object UnitConstant {

  def quoted(using quotes: Quotes): UnitConstant.Module = quotes.reflectV2.UnitConstant
  given moduleConversion: (quotes: Quotes) => Conversion[UnitConstant.type, UnitConstant.Module] = _ => quotes.reflectV2.UnitConstant

  def unapply(x: UnitConstant): true = true

  trait Module private[compiletime] () {
    def apply(): UnitConstant
    def make(): UnitConstant
  }

}

/////// NullConstant ///////////////////////////////////////////////////////////////

trait NullConstant private[compiletime] () extends Constant {

  override def value: Null

}
object NullConstant {

  def quoted(using quotes: Quotes): NullConstant.Module = quotes.reflectV2.NullConstant
  given moduleConversion: (quotes: Quotes) => Conversion[NullConstant.type, NullConstant.Module] = _ => quotes.reflectV2.NullConstant

  def unapply(x: NullConstant): true = true

  trait Module private[compiletime] () {
    def apply(): NullConstant
    def make(): NullConstant
  }

}

/////// ClassOfConstant ///////////////////////////////////////////////////////////////

trait ClassOfConstant private[compiletime] () extends Constant {

  override def value: TypeRepr

}
object ClassOfConstant {

  def quoted(using quotes: Quotes): ClassOfConstant.Module = quotes.reflectV2.ClassOfConstant
  given moduleConversion: (quotes: Quotes) => Conversion[ClassOfConstant.type, ClassOfConstant.Module] = _ => quotes.reflectV2.ClassOfConstant

  def unapply(x: ClassOfConstant): Some[TypeRepr] = Some(x.value)

  trait Module private[compiletime] () {
    def apply(x: TypeRepr): ClassOfConstant
    def make(x: TypeRepr): ClassOfConstant
  }

}
