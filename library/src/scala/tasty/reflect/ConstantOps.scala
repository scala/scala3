package scala.tasty
package reflect

trait ConstantOps extends Core {

  trait ConstantAPI {
    def value: Any
  }
  implicit def ConstantDeco(const: Constant): ConstantAPI

  /** Module of Constant literals */
  val Constant: ConstantModule
  abstract class ConstantModule {

    /** Module of Null literals */
    val Unit: UnitModule
    abstract class UnitModule {
      /** Extractor for Unit literals */
      def unapply(constant: Constant): Boolean
    }

    /** Module of Null literals */
    val Null: NullModule
    abstract class NullModule {
      /** Extractor for Null literals */
      def unapply(constant: Constant): Boolean
    }

    /** Module of Boolean literals */
    val Boolean: BooleanModule
    abstract class BooleanModule {
      /** Extractor for Boolean literals */
      def unapply(constant: Constant): Option[Boolean]
    }

    /** Module of Byte literals */
    val Byte: ByteModule
    abstract class ByteModule {
      /** Extractor for Byte literals */
      def unapply(constant: Constant): Option[Byte]
    }

    /** Module of Short literals */
    val Short: ShortModule
    abstract class ShortModule {
      /** Extractor for Short literals */
      def unapply(constant: Constant): Option[Short]
    }

    /** Module of Char literals */
    val Char: CharModule
    abstract class CharModule {
      /** Extractor for Char literals */
      def unapply(constant: Constant): Option[Char]
    }

    /** Module of Int literals */
    val Int: IntModule
    abstract class IntModule {
      /** Extractor for Int literals */
      def unapply(constant: Constant): Option[Int]
    }

    /** Module of Long literals */
    val Long: LongModule
    abstract class LongModule {
      /** Extractor for Long literals */
      def unapply(constant: Constant): Option[Long]
    }

    /** Module of Float literals */
    val Float: FloatModule
    abstract class FloatModule {
      /** Extractor for Float literals */
      def unapply(constant: Constant): Option[Float]
    }

    /** Module of Double literals */
    val Double: DoubleModule
    abstract class DoubleModule {
      /** Extractor for Double literals */
      def unapply(constant: Constant): Option[Double]
    }

    /** Module of String literals */
    val String: StringModule
    abstract class StringModule {
      /** Extractor for String literals */
      def unapply(constant: Constant): Option[String]
    }

    /** Module of ClassTag literals */
    val ClassTag: ClassTagModule
    abstract class ClassTagModule {
      /** Extractor for ClassTag literals */
      def unapply(constant: Constant): Option[Type]
    }

    /** Module of scala.Symbol literals */
    val Symbol: SymbolModule
    /** Extractor for scala.Symbol literals */
    abstract class SymbolModule {
      def unapply(constant: Constant): Option[scala.Symbol]
    }
  }
}
