package dotty.runtime.vc

abstract class VCPrototype {
}


abstract class VCArrayPrototype[T <: VCPrototype] extends Object with Cloneable {
  def apply(idx: Int): Object
  def update(idx: Int, el: T): Unit
  def length: Int
  override def clone: Object = super.clone()
}
