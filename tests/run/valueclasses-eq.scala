trait HasEquals extends Any {
  override def equals(other: Any): Boolean = false
}
trait HasEq extends Any {
  def ==(other: VCCustomEqInTrait): Boolean = false
}

trait HasNe extends Any {
  def !=(other: VCCustomNeInTrait): Boolean = false
}

class VCNonCustom(val x: Int) extends AnyVal
object VCNonCustom

class VCCustomEquals1(val x: Int) extends AnyVal {
  override def equals(other: Any): Boolean = false
}
object VCCustomEquals1

class VCCustomEquals2(val x: Int) extends AnyVal with HasEquals
object VCCustomEquals2

class VCCustomEq(val x: Int) extends AnyVal {
  def ==(other: VCCustomEq): Boolean = false
}
object VCCustomEq

class VCCustomNe(val x: Int) extends AnyVal {
  def !=(other: VCCustomNe): Boolean = false
}
object VCCustomNe

class VCCustomEqInTrait(val x: Int) extends AnyVal with HasEq
object VCCustomEqInTrait

class VCCustomNeInTrait(val x: Int) extends AnyVal with HasNe
object VCCustomNeInTrait

object Test {
  /** Does the class `vc` contain the methods:
   *  def ==(other: erasedVC)
   *  def !=(other: erasedVC)
   */
  def hasOverloadedEq(vc: Class, erasedVC: Class) =
    try {
      // == is encoded as $eq$eq
      vc.getMethod("$eq$eq", erasedVC)
      // != is encoded as $bang$eq
      vc.getMethod("$bang$eq", erasedVC)
      true
    } catch {
      case _: NoSuchMethodException => false
    }

  /** Does the companion class `companion` the methods:
   *  def ==$extension($this: erasedVC, other: erasedVC)
   *  def !=$extension($this: erasedVC, other: erasedVC)
   */
  def hasExtensionEq(companion: Class, erasedVC: Class) =
    try {
      companion.getMethod("$eq$eq$extension", erasedVC, erasedVC)
      companion.getMethod("$bang$eq$extension", erasedVC, erasedVC)
      true
    } catch {
      case _: NoSuchMethodException => false
    }

  def main(args: Array[String]): Unit = {
    // The overloaded == and != should always be defined in value classes
    assert(hasOverloadedEq(classOf[VCNonCustom], classOf[Int]))
    assert(hasOverloadedEq(classOf[VCCustomEquals1], classOf[Int]))
    assert(hasOverloadedEq(classOf[VCCustomEquals2], classOf[Int]))
    assert(hasOverloadedEq(classOf[VCCustomEq], classOf[Int]))
    assert(hasOverloadedEq(classOf[VCCustomEqInTrait], classOf[Int]))


    // The extension method corresponding to the overloaded == should
    // always be defined, except when the overloaded == is defined
    // in a universal trait (because we don't have extension methods
    // in universal traits yet). The same apply to !=
    assert(hasExtensionEq(VCNonCustom.getClass, classOf[Int]))
    assert(hasExtensionEq(VCCustomEquals1.getClass, classOf[Int]))
    assert(hasExtensionEq(VCCustomEquals2.getClass, classOf[Int]))
    assert(hasExtensionEq(VCCustomEq.getClass, classOf[Int]))
    assert(hasExtensionEq(VCCustomNe.getClass, classOf[Int]))

    assert(!hasExtensionEq(VCCustomEqInTrait.getClass, classOf[Int]))
    assert(!hasExtensionEq(VCCustomNeInTrait.getClass, classOf[Int]))
  }
}