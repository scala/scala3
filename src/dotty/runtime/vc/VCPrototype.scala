package dotty.runtime.vc

abstract class VCPrototype {
}

abstract class VCCompanionPrototype {

}

abstract class VCArrayPrototype[T <: VCPrototype] {
  def apply(idx: Int): Object
  def update(idx: Int, el: T): Unit
  def length: Int
}
