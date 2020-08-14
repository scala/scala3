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

enum EO:
  case H
  case I(arg: Int)
  // TODO: allow `productPrefix` to be overridden in singleton enum values - until `scala.Enum` is bootstrapped with
  // `enumLabel`, `runtime.EnumValues` uses `productPrefix` for by-name lookup.
  override def productPrefix: String = "noprefix"
end EO

enum EL {
  case J
  case K(arg: Int)
  override def enumLabel: String = "nolabel" // will always be overridden by simple cases
}

enum EQ[T] {
  case L extends EQ[Int]
  override def enumLabel: String = "nolabel" // will always be overridden by value cases
}

abstract class Tag[T] extends Enum
object Tag:
  private final class IntTagImpl extends Tag[Int] with runtime.EnumValue:
    def ordinal = 0
    def enumLabel = "IntTag"
    override def hashCode = 123
  final val IntTag: Tag[Int] = IntTagImpl()

@main def Test =
  assert(ES.A.toString == "overridden",       s"ES.A.toString = ${ES.A.toString}")
  assert(ES.A.productPrefix == "A",           s"ES.A.productPrefix = ${ES.A.productPrefix}")
  assert(ES.A.enumLabel == "A",               s"ES.A.enumLabel = ${ES.A.enumLabel}")
  assert(ES.valueOf("A") == ES.A,             s"ES.valueOf(A) = ${ES.valueOf("A")}")
  assert(EJ.B.toString == "overridden",       s"EJ.B.toString = ${EJ.B.toString}")
  assert(EJ.B.productPrefix == "B",           s"EJ.B.productPrefix = ${EJ.B.productPrefix}")
  assert(EJ.B.enumLabel == "B",               s"EJ.B.enumLabel = ${EJ.B.enumLabel}")
  assert(EJ.valueOf("B") == EJ.B,             s"EJ.valueOf(B) = ${EJ.valueOf("B")}")
  assert(EM.C.toString == "overridden",       s"EM.C.toString = ${EM.C.toString}")
  assert(EM.C.productPrefix == "C",           s"EM.C.productPrefix = ${EM.C.productPrefix}")
  assert(EM.C.enumLabel == "C",               s"EM.C.enumLabel = ${EM.C.enumLabel}")
  assert(EM.valueOf("C") == EM.C,             s"EM.valueOf(C) = ${EM.valueOf("C")}")
  assert(ET.D.toString == "overridden",       s"ET.D.toString = ${ET.D.toString}")
  assert(ET.D.productPrefix == "D",           s"ET.D.productPrefix = ${ET.D.productPrefix}")
  assert(ET.D.enumLabel == "D",               s"ET.D.enumLabel = ${ET.D.enumLabel}")
  assert(EZ.E(0).toString == "overridden",    s"EZ.E(0).toString = ${EZ.E(0).toString}")
  assert(EZ.E(0).productPrefix == "E",        s"EZ.E(0).productPrefix = ${EZ.E(0).productPrefix}")
  assert(EZ.E(0).enumLabel == "E",            s"EZ.E(0).enumLabel = ${EZ.E(0).enumLabel}")
  assert(EC.F.toString == "F",                s"EC.F.toString = ${EC.F.toString}")
  assert(EC.F.productPrefix == "F",           s"EC.F.productPrefix = ${EC.F.productPrefix}")
  assert(EC.F.enumLabel == "F",               s"EC.F.enumLabel = ${EC.F.enumLabel}")
  assert(EC.valueOf("F") == EC.F,             s"EC.valueOf(F) = ${EC.valueOf("F")}")
  assert(EC.G(0).toString == "G(0)",          s"EC.G(0).toString = ${EC.G(0).toString}")
  assert(EC.G(0).productPrefix == "G",        s"EC.G(0).productPrefix = ${EC.G(0).productPrefix}")
  assert(EC.G(0).enumLabel == "G",            s"EC.G(0).enumLabel = ${EC.G(0).enumLabel}")
  assert(EO.H.toString == "H",                s"EO.H.toString = ${EO.H.toString}")
  assert(EO.H.productPrefix == "H",           s"EO.H.productPrefix = ${EO.H.productPrefix}") // TODO: enable override
  assert(EO.H.enumLabel == "H",               s"EO.H.enumLabel = ${EO.H.enumLabel}")
  assert(EO.valueOf("H") == EO.H,             s"EO.valueOf(H) = ${EO.valueOf("H")}")
  assert(EO.I(0).toString == "noprefix(0)",   s"EO.I(0).toString = ${EO.I(0).toString}")
  assert(EO.I(0).productPrefix == "noprefix", s"EO.I(0).productPrefix = ${EO.I(0).productPrefix}")
  assert(EO.I(0).enumLabel == "I",            s"EO.I(0).enumLabel = ${EO.I(0).enumLabel}")
  assert(EL.J.toString == "J",                s"EL.J.toString = ${EL.J.toString}")
  assert(EL.J.productPrefix == "J",           s"EL.J.productPrefix = ${EL.J.productPrefix}")
  assert(EL.J.enumLabel == "J",               s"EL.J.enumLabel = ${EL.J.enumLabel}") // can't override label in simple case
  assert(EL.valueOf("J") == EL.J,             s"EL.valueOf(J) = ${EL.valueOf("J")}")
  assert(EL.K(0).toString == "K(0)",          s"EL.K(0).toString = ${EL.K(0).toString}")
  assert(EL.K(0).productPrefix == "K",        s"EL.K(0).productPrefix = ${EL.K(0).productPrefix}")
  assert(EL.K(0).enumLabel == "nolabel",      s"EL.K(0).enumLabel = ${EL.K(0).enumLabel}") // enum label overridden in class case
  assert(EQ.L.toString == "L",                s"EQ.L.toString = ${EQ.L.toString}")
  assert(EQ.L.productPrefix == "L",           s"EQ.L.productPrefix = ${EQ.L.productPrefix}")
  assert(EQ.L.enumLabel == "L",               s"EQ.L.enumLabel = ${EQ.L.enumLabel}") // can't override label in value case

  assert(
    assertion = Tag.IntTag.toString == s"${Tag.IntTag.getClass.getName}@${Integer.toHexString(123)}",
    message   = s"Tag.IntTag.toString = ${Tag.IntTag.toString}"
  )
  assert(Tag.IntTag.productPrefix == Tag.IntTag.toString, s"Tag.IntTag.productPrefix = ${Tag.IntTag.productPrefix}")
  assert(Tag.IntTag.enumLabel     == "IntTag", s"Tag.IntTag.enumLabel = ${Tag.IntTag.enumLabel}")
