package scala.tasty
package reflect

trait ConstantOps extends Core {

  implicit class ConstantAPI(const: Constant) {
    def value: Any = kernel.Constant_value(const)
  }

  /** Module of Constant literals */
  val Constant: ConstantModule
  abstract class ConstantModule {

    /** Module of Null literals */
    val Unit: UnitModule
    abstract class UnitModule {
      /** Unit `()` literal */
      def apply(): Constant

      /** Extractor for Unit literals */
      def unapply(constant: Constant): Boolean
    }

    /** Module of Null literals */
    val Null: NullModule
    abstract class NullModule {
      /** `null` literal */
      def apply(): Constant

      /** Extractor for Null literals */
      def unapply(constant: Constant): Boolean
    }

    /** Module of Boolean literals */
    val Boolean: BooleanModule
    abstract class BooleanModule {
      /** Boolean literal */
      def apply(x: Boolean): Constant

      /** Extractor for Boolean literals */
      def unapply(constant: Constant): Option[Boolean]
    }

    /** Module of Byte literals */
    val Byte: ByteModule
    abstract class ByteModule {
      /** Byte literal */
      def apply(x: Byte): Constant

      /** Extractor for Byte literals */
      def unapply(constant: Constant): Option[Byte]
    }

    /** Module of Short literals */
    val Short: ShortModule
    abstract class ShortModule {
      /** Short literal */
      def apply(x: Short): Constant

      /** Extractor for Short literals */
      def unapply(constant: Constant): Option[Short]
    }

    /** Module of Char literals */
    val Char: CharModule
    abstract class CharModule {
      /** Char literal */
      def apply(x: Char): Constant

      /** Extractor for Char literals */
      def unapply(constant: Constant): Option[Char]
    }

    /** Module of Int literals */
    val Int: IntModule
    abstract class IntModule {
      /** Int literal */
      def apply(x: Int): Constant

      /** Extractor for Int literals */
      def unapply(constant: Constant): Option[Int]
    }

    /** Module of Long literals */
    val Long: LongModule
    abstract class LongModule {
      /** Long literal */
      def apply(x: Long): Constant

      /** Extractor for Long literals */
      def unapply(constant: Constant): Option[Long]
    }

    /** Module of Float literals */
    val Float: FloatModule
    abstract class FloatModule {
      /** Float literal */
      def apply(x: Float): Constant

      /** Extractor for Float literals */
      def unapply(constant: Constant): Option[Float]
    }

    /** Module of Double literals */
    val Double: DoubleModule
    abstract class DoubleModule {
      /** Double literal */
      def apply(x: Double): Constant

      /** Extractor for Double literals */
      def unapply(constant: Constant): Option[Double]
    }

    /** Module of String literals */
    val String: StringModule
    abstract class StringModule {
      /** String literal */
      def apply(x: String): Constant

      /** Extractor for String literals */
      def unapply(constant: Constant): Option[String]
    }

    /** Module of ClassTag literals */
    val ClassTag: ClassTagModule
    abstract class ClassTagModule {
      /** scala.reflect.ClassTag literal */
      def apply[T](implicit x: scala.reflect.ClassTag[T]): Constant

      /** Extractor for ClassTag literals */
      def unapply(constant: Constant): Option[Type]
    }

    /** Module of scala.Symbol literals */
    val Symbol: SymbolModule
    /** Extractor for scala.Symbol literals */
    abstract class SymbolModule {
      /** scala.Symbol literal */
      def apply(x: scala.Symbol): Constant

      /** Extractor for scala.Symbol literals */
      def unapply(constant: Constant): Option[scala.Symbol]
    }
  }
}
