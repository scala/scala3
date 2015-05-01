package dotty.runtime.vc

abstract class VCPrototype {
}

abstract class VCArrayPrototype[T <: VCPrototype] extends Cloneable {
  def apply(idx: Int): Object
  def update(idx: Int, el: T): Unit
  def length: Int
}
