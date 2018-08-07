package scala.tasty
package reflect

trait ConstantOps extends TastyCore {

  trait ConstantAPI {
    def value: Any
  }
  implicit def ConstantDeco(const: Constant): ConstantAPI

  val Constant: ConstantModule
  abstract class ConstantModule {

    val Unit: UnitExtractor
    abstract class UnitExtractor {
      def unapply(constant: Constant): Boolean
    }

    val Null: NullExtractor
    abstract class NullExtractor {
      def unapply(constant: Constant): Boolean
    }

    val Boolean: BooleanExtractor
    abstract class BooleanExtractor {
      def unapply(constant: Constant): Option[Boolean]
    }

    val Byte: ByteExtractor
    abstract class ByteExtractor {
      def unapply(constant: Constant): Option[Byte]
    }

    val Short: ShortExtractor
    abstract class ShortExtractor {
      def unapply(constant: Constant): Option[Short]
    }

    val Char: CharExtractor
    abstract class CharExtractor {
      def unapply(constant: Constant): Option[Char]
    }

    val Int: IntExtractor
    abstract class IntExtractor {
      def unapply(constant: Constant): Option[Int]
    }

    val Long: LongExtractor
    abstract class LongExtractor {
      def unapply(constant: Constant): Option[Long]
    }

    val Float: FloatExtractor
    abstract class FloatExtractor {
      def unapply(constant: Constant): Option[Float]
    }

    val Double: DoubleExtractor
    abstract class DoubleExtractor {
      def unapply(constant: Constant): Option[Double]
    }

    val String: StringExtractor
    abstract class StringExtractor {
      def unapply(constant: Constant): Option[String]
    }

    val ClassTag: ClassTagExtractor
    abstract class ClassTagExtractor {
      def unapply(constant: Constant): Option[Type]
    }

    /** Extractor for scala.Symbol literals */
    val Symbol: SymbolExtractor
    /** Extractor for scala.Symbol literals */
    abstract class SymbolExtractor {
      def unapply(constant: Constant): Option[scala.Symbol]
    }
  }
}
