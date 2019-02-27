package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Constants


trait ConstantOpsImpl extends scala.tasty.reflect.ConstantOps with CoreImpl {

  object Constant extends ConstantModule {

    object Unit extends UnitModule {
      def apply(): Constant = Constants.Constant(())
      def unapply(x: Constant): Boolean = x.tag == Constants.UnitTag
    }

    object Null extends NullModule {
      def apply(): Constant = Constants.Constant(null)
      def unapply(x: Constant): Boolean = x.tag == Constants.NullTag
    }

    object Boolean extends BooleanModule {
      def apply(x: Boolean): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Boolean] = x match {
        case x: Constants.Constant if x.tag == Constants.BooleanTag => Some(x.booleanValue)
        case _ => None
      }
    }

    object Byte extends ByteModule {
      def apply(x: Byte): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Byte] = x match {
        case x: Constants.Constant if x.tag == Constants.ByteTag => Some(x.byteValue)
        case _ => None
      }
    }

    object Short extends ShortModule {
      def apply(x: Short): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Short] = x match {
        case x: Constants.Constant if x.tag == Constants.ShortTag => Some(x.shortValue)
        case _ => None
      }
    }

    object Char extends CharModule {
      def apply(x: Char): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Char] = x match {
        case x: Constants.Constant if x.tag == Constants.CharTag => Some(x.charValue)
        case _ => None
      }
    }

    object Int extends IntModule {
      def apply(x: Int): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Int] = x match {
        case x: Constants.Constant if x.tag == Constants.IntTag => Some(x.intValue)
        case _ => None
      }
    }

    object Long extends LongModule {
      def apply(x: Long): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Long] = x match {
        case x: Constants.Constant if x.tag == Constants.LongTag => Some(x.longValue)
        case _ => None
      }
    }

    object Float extends FloatModule {
      def apply(x: Float): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Float] = x match {
        case x: Constants.Constant if x.tag == Constants.FloatTag => Some(x.floatValue)
        case _ => None
      }
    }

    object Double extends DoubleModule {
      def apply(x: Double): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Double] = x match {
        case x: Constants.Constant if x.tag == Constants.DoubleTag => Some(x.doubleValue)
        case _ => None
      }
    }

    object String extends StringModule {
      def apply(x: String): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[String] = x match {
        case x: Constants.Constant if x.tag == Constants.StringTag => Some(x.stringValue)
        case _ => None
      }
    }

    object ClassTag extends ClassTagModule {
      def apply[T](implicit x: scala.reflect.ClassTag[T]): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[Type] = x match {
        case x: Constants.Constant if x.tag == Constants.ClazzTag => Some(x.typeValue)
        case _ => None
      }
    }

    object Symbol extends SymbolModule {
      def apply(x: scala.Symbol): Constant = Constants.Constant(x)
      def unapply(x: Constant): Option[scala.Symbol] = x match {
        case x: Constants.Constant if x.tag == Constants.ScalaSymbolTag => Some(x.scalaSymbolValue)
        case _ => None
      }
    }
  }

}
