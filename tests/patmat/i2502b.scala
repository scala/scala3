abstract class BTypes {
  trait BType

  sealed trait RefBType extends BType {
    def classOrArrayType: String = this match {
      case ClassBType(internalName) => internalName
      case a: ArrayBType            => ""
    }
  }

  final case class ClassBType(val internalName: String) extends RefBType
  class ArrayBType extends RefBType

  object ClassBType {
    def unapply(x: RefBType): Option[String] = None
  }
}
