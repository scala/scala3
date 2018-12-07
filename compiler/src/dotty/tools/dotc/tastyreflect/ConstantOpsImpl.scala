package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Constants


trait ConstantOpsImpl extends scala.tasty.reflect.ConstantOps with CoreImpl {

  def ConstantDeco(const: Constant): ConstantAPI = new ConstantAPI {
    def value: Any = const.value
  }

  object Constant extends ConstantModule {

    object Unit extends UnitModule {
      def unapply(x: Constant): Boolean = x.tag == Constants.UnitTag
    }

    object Null extends NullModule {
      def unapply(x: Constant): Boolean = x.tag == Constants.NullTag
    }

    object Boolean extends BooleanModule {
      def unapply(x: Constant): Option[Boolean] = x match {
        case x: Constants.Constant if x.tag == Constants.BooleanTag => Some(x.booleanValue)
        case _ => None
      }
    }

    object Byte extends ByteModule {
      def unapply(x: Constant): Option[Byte] = x match {
        case x: Constants.Constant if x.tag == Constants.ByteTag => Some(x.byteValue)
        case _ => None
      }
    }

    object Short extends ShortModule {
      def unapply(x: Constant): Option[Short] = x match {
        case x: Constants.Constant if x.tag == Constants.ShortTag => Some(x.shortValue)
        case _ => None
      }
    }

    object Char extends CharModule {
      def unapply(x: Constant): Option[Char] = x match {
        case x: Constants.Constant if x.tag == Constants.CharTag => Some(x.charValue)
        case _ => None
      }
    }

    object Int extends IntModule {
      def unapply(x: Constant): Option[Int] = x match {
        case x: Constants.Constant if x.tag == Constants.IntTag => Some(x.intValue)
        case _ => None
      }
    }

    object Long extends LongModule {
      def unapply(x: Constant): Option[Long] = x match {
        case x: Constants.Constant if x.tag == Constants.LongTag => Some(x.longValue)
        case _ => None
      }
    }

    object Float extends FloatModule {
      def unapply(x: Constant): Option[Float] = x match {
        case x: Constants.Constant if x.tag == Constants.FloatTag => Some(x.floatValue)
        case _ => None
      }
    }

    object Double extends DoubleModule {
      def unapply(x: Constant): Option[Double] = x match {
        case x: Constants.Constant if x.tag == Constants.DoubleTag => Some(x.doubleValue)
        case _ => None
      }
    }

    object String extends StringModule {
      def unapply(x: Constant): Option[String] = x match {
        case x: Constants.Constant if x.tag == Constants.StringTag => Some(x.stringValue)
        case _ => None
      }
    }

    object ClassTag extends ClassTagModule {
      def unapply(x: Constant): Option[Type] = x match {
        case x: Constants.Constant if x.tag == Constants.ClazzTag => Some(x.typeValue)
        case _ => None
      }
    }

    object Symbol extends SymbolModule {
      def unapply(x: Constant): Option[scala.Symbol] = x match {
        case x: Constants.Constant if x.tag == Constants.ScalaSymbolTag => Some(x.scalaSymbolValue)
        case _ => None
      }
    }
  }

}
