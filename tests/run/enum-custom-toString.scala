enum ES:
  case A
  override def toString: String = "overridden"

enum EJ extends java.lang.Enum[EJ]:
  case B
  override def toString: String = "overridden"

trait Mixin:
  override def toString: String = "overridden"

enum EM extends Mixin:
  case C

enum ET[T] extends java.lang.Enum[ET[_]]:
  case D extends ET[Unit]
  override def toString: String = "overridden"

enum EZ:
  case E(arg: Int)
  override def toString: String = "overridden"

enum EC: // control case
  case F
  case G(arg: Int)

abstract class Tag[T] extends Enum
object Tag:
  private final class IntTagImpl extends Tag[Int] with runtime.EnumValue:
    def ordinal = 0
    override def hashCode = 123
  final val IntTag: Tag[Int] = IntTagImpl()

@main def Test =
  assert(ES.A.toString == "overridden",    s"ES.A.toString = ${ES.A.toString}")
  assert(ES.A.productPrefix == "A",        s"ES.A.productPrefix = ${ES.A.productPrefix}")
  assert(ES.A.enumLabel == "A",            s"ES.A.enumLabel = ${ES.A.enumLabel}")
  assert(ES.valueOf("A") == ES.A,          s"ES.valueOf(A) = ${ES.valueOf("A")}")
  assert(EJ.B.toString == "overridden",    s"EJ.B.toString = ${EJ.B.toString}")
  assert(EJ.B.productPrefix == "B",        s"EJ.B.productPrefix = ${EJ.B.productPrefix}")
  assert(EJ.B.enumLabel == "B",            s"EJ.B.enumLabel = ${EJ.B.enumLabel}")
  assert(EJ.valueOf("B") == EJ.B,          s"EJ.valueOf(B) = ${EJ.valueOf("B")}")
  assert(EM.C.toString == "overridden",    s"EM.C.toString = ${EM.C.toString}")
  assert(EM.C.productPrefix == "C",        s"EM.C.productPrefix = ${EM.C.productPrefix}")
  assert(EM.C.enumLabel == "C",            s"EM.C.enumLabel = ${EM.C.enumLabel}")
  assert(EM.valueOf("C") == EM.C,          s"EM.valueOf(C) = ${EM.valueOf("C")}")
  assert(ET.D.toString == "overridden",    s"ET.D.toString = ${ET.D.toString}")
  assert(ET.D.productPrefix == "D",        s"ET.D.productPrefix = ${ET.D.productPrefix}")
  assert(ET.D.enumLabel == "D",            s"ET.D.enumLabel = ${ET.D.enumLabel}")
  assert(EZ.E(0).toString == "overridden", s"EZ.E(0).toString = ${EZ.E(0).toString}")
  assert(EZ.E(0).productPrefix == "E",     s"EZ.E(0).productPrefix = ${EZ.E(0).productPrefix}")
  assert(EZ.E(0).enumLabel == "E",         s"EZ.E(0).enumLabel = ${EZ.E(0).enumLabel}")
  assert(EC.F.toString == "F",             s"EC.F.toString = ${EC.F.toString}")
  assert(EC.F.productPrefix == "F",        s"EC.F.productPrefix = ${EC.F.productPrefix}")
  assert(EC.F.enumLabel == "F",            s"EC.F.enumLabel = ${EC.F.enumLabel}")
  assert(EC.valueOf("F") == EC.F,          s"EC.valueOf(F) = ${EC.valueOf("F")}")
  assert(EC.G(0).toString == "G(0)",       s"EC.G(0).toString = ${EC.G(0).toString}")
  assert(EC.G(0).productPrefix == "G",     s"EC.G(0).productPrefix = ${EC.G(0).productPrefix}")
  assert(EC.G(0).enumLabel == "G",         s"EC.G(0).enumLabel = ${EC.G(0).enumLabel}")

  assert(
    assertion = Tag.IntTag.toString == s"${Tag.IntTag.getClass.getName}@${Integer.toHexString(123)}",
    message   = s"Tag.IntTag.toString = ${Tag.IntTag.toString}"
  )
  assert(Tag.IntTag.productPrefix == Tag.IntTag.toString, s"Tag.IntTag.productPrefix = ${Tag.IntTag.productPrefix}")
  assert(Tag.IntTag.enumLabel     == Tag.IntTag.toString, s"Tag.IntTag.enumLabel = ${Tag.IntTag.enumLabel}")
