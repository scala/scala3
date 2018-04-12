package scala.tasty

package object types {

  object TypeBounds {
    type Data = (Type, Type)
    def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeBounds(arg)
  }


}
