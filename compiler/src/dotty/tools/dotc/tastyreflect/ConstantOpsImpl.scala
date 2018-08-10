package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Constants

import scala.tasty.util.Show

trait ConstantOpsImpl extends scala.tasty.reflect.ConstantOps with TastyCoreImpl {

  def ConstantDeco(const: Constant): ConstantAPI = new ConstantAPI {
    def value: Any = const.value
  }

  object Constant extends ConstantModule {

    object Unit extends UnitExtractor {
      def unapply(x: Constant): Boolean = x match {
        case x: Constants.Constant => x.tag == Constants.UnitTag
        case _ => false
      }
    }

    object Null extends NullExtractor {
      def unapply(x: Constant): Boolean =  x match {
        case x: Constants.Constant => x.tag == Constants.NullTag
        case _ => false
      }
    }

    object Boolean extends BooleanExtractor {
      def unapply(x: Constant): Option[Boolean] = x match {
        case x: Constants.Constant if x.tag == Constants.BooleanTag => Some(x.booleanValue)
        case _ => None
      }
    }

    object Byte extends ByteExtractor {
      def unapply(x: Constant): Option[Byte] = x match {
        case x: Constants.Constant if x.tag == Constants.ByteTag => Some(x.byteValue)
        case _ => None
      }
    }

    object Short extends ShortExtractor {
      def unapply(x: Constant): Option[Short] = x match {
        case x: Constants.Constant if x.tag == Constants.ShortTag => Some(x.shortValue)
        case _ => None
      }
    }

    object Char extends CharExtractor {
      def unapply(x: Constant): Option[Char] = x match {
        case x: Constants.Constant if x.tag == Constants.CharTag => Some(x.charValue)
        case _ => None
      }
    }

    object Int extends IntExtractor {
      def unapply(x: Constant): Option[Int] = x match {
        case x: Constants.Constant if x.tag == Constants.IntTag => Some(x.intValue)
        case _ => None
      }
    }

    object Long extends LongExtractor {
      def unapply(x: Constant): Option[Long] = x match {
        case x: Constants.Constant if x.tag == Constants.LongTag => Some(x.longValue)
        case _ => None
      }
    }

    object Float extends FloatExtractor {
      def unapply(x: Constant): Option[Float] = x match {
        case x: Constants.Constant if x.tag == Constants.FloatTag => Some(x.floatValue)
        case _ => None
      }
    }

    object Double extends DoubleExtractor {
      def unapply(x: Constant): Option[Double] = x match {
        case x: Constants.Constant if x.tag == Constants.DoubleTag => Some(x.doubleValue)
        case _ => None
      }
    }

    object String extends StringExtractor {
      def unapply(x: Constant): Option[String] = x match {
        case x: Constants.Constant if x.tag == Constants.StringTag => Some(x.stringValue)
        case _ => None
      }
    }

    object ClassTag extends ClassTagExtractor {
      def unapply(x: Constant): Option[Type] = x match {
        case x: Constants.Constant if x.tag == Constants.ClazzTag => Some(x.typeValue)
        case _ => None
      }
    }

    object Symbol extends SymbolExtractor {
      def unapply(x: Constant): Option[scala.Symbol] = x match {
        case x: Constants.Constant if x.tag == Constants.ScalaSymbolTag => Some(x.scalaSymbolValue)
        case _ => None
      }
    }
  }

}
