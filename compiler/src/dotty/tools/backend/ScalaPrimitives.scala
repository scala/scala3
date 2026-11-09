package dotty.tools.backend

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.{MutableSymbolMap, NoSymbol, Symbol, defn}
import dotty.tools.dotc.ast.Trees.Select
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.util.{ReadOnlyMap, EqHashMap}
import dotty.tools.backend.ScalaPrimitivesOps.*

import scala.annotation.constructorOnly

/** Scala primitive operations are represented as methods in `Any` and
 *  `AnyVal` subclasses. Here we demultiplex them by providing a mapping
 *  from their symbols to integers. Different methods exist for
 *  different value types, but with the same meaning (like plus, minus,
 *  etc.). They will all be mapped to the same int.
 *
 *  Note: The three equal methods have the following semantics:
 *  - `"=="` checks for `null`, and if non-null, calls
 *    `java.lang.Object.equals`
 *    `(class: Any; modifier: final)`. Primitive: `EQ`
 *  - `"eq"` usual reference comparison
 *    `(class: AnyRef; modifier: final)`. Primitive: `ID`
 *  - `"equals"` user-defined equality (Java semantics)
 *    `(class: Object; modifier: none)`. Primitive: `EQUALS`
 *
 * Inspired from the `scalac` compiler.
 */
class ScalaPrimitives(using @constructorOnly initCtx: Context) {

  private val primitives: ReadOnlyMap[Symbol, ReadOnlyMap[Name, Int]] = init

  /**
   * Checks whether the given symbol has an associated primitive code.
   * Only use when you do not need the code at all! Otherwise, call `getPrimitive` to avoid a double lookup.
   */
  final def isPrimitive(sym: Symbol)(using Context): Boolean =
    getPrimitive(sym).isDefined

  /** Returns the primitive code for the given symbol. */
  def getPrimitive(sym: Symbol)(using Context): Option[Int] = {
    primitives.get(sym.owner) match
      case None => None
      case Some(m) => m.get(sym.name) match
        case None => None
        case Some(code) =>
          if code == ADD then
            sym.info.paramInfoss match
              case (tp :: _) :: Nil if tp =:= defn.StringType => Some(CONCAT)
              case _ => Some(code)
          else
            Some(code)
  }

  /** Returns the primitive code for the given tree. */
  def getPrimitive(fun: Tree)(using Context): Option[Int] =
    val sym = fun.symbol
    if sym == NoSymbol then
      // the only trees that do not have a symbol assigned are array.{update,select,length,clone}
      fun match
        case Select(_, nme.primitive.arrayUpdate) => Some(UPDATE)
        case Select(_, nme.primitive.arrayLength) => Some(LENGTH)
        case Select(_, nme.primitive.arrayApply) => Some(APPLY)
        case Select(_, nme.clone_) => None // but array.clone is NOT a primitive op
        case Apply(s, _) => getPrimitive(s)
        case _ => throw new AssertionError("Tried to get the primitive of an unknown function without a symbol: " + fun)
    else
      getPrimitive(sym)

  /** Initializes the primitive map */
  private def init(using Context): ReadOnlyMap[Symbol, ReadOnlyMap[Name, Int]] = {
    val primitives = MutableSymbolMap[ReadOnlyMap[Name, Int]]()

    // scala.Any
    val anyPrimitives = EqHashMap[Name, Int]()
    anyPrimitives(nme.EQ) = EQ
    anyPrimitives(nme.NE) = NE
    anyPrimitives(nme.isInstanceOf_) = IS
    anyPrimitives(nme.asInstanceOf_) = AS
    anyPrimitives(nme.HASHHASH) = HASH
    primitives(defn.AnyClass) = anyPrimitives

    // java.lang.Object
    val objectPrimitives = EqHashMap[Name, Int]()
    objectPrimitives(nme.EQ) = EQ
    objectPrimitives(nme.NE) = NE
    objectPrimitives(nme.eq) = ID
    objectPrimitives(nme.ne) = NI
    objectPrimitives(nme.synchronized_) = SYNCHRONIZED
    primitives(defn.ObjectClass) = objectPrimitives

    // java.lang.String
    val stringPrimitives = EqHashMap[Name, Int]()
    stringPrimitives(nme.PLUS) = CONCAT
    primitives(defn.StringClass) = stringPrimitives

    // scala.Array
    val arrayPrimitives = EqHashMap[Name, Int]()
    arrayPrimitives(nme.length) = LENGTH
    arrayPrimitives(nme.apply) = APPLY
    arrayPrimitives(nme.update) = UPDATE
    primitives(defn.ArrayClass) = arrayPrimitives

    // scala.Boolean
    val booleanPrimitives = EqHashMap[Name, Int]()
    booleanPrimitives(nme.EQ) = EQ
    booleanPrimitives(nme.NE) = NE
    booleanPrimitives(nme.UNARY_!) = ZNOT
    booleanPrimitives(nme.ZOR) = ZOR
    booleanPrimitives(nme.ZAND) = ZAND
    booleanPrimitives(nme.OR) = OR
    booleanPrimitives(nme.AND) = AND
    booleanPrimitives(nme.XOR) = XOR
    primitives(defn.BooleanClass) = booleanPrimitives

    // scala.Byte
    val bytePrimitives = EqHashMap[Name, Int]()
    bytePrimitives(nme.EQ) = EQ
    bytePrimitives(nme.NE) = NE
    bytePrimitives(nme.ADD) = ADD
    bytePrimitives(nme.SUB) = SUB
    bytePrimitives(nme.MUL) = MUL
    bytePrimitives(nme.DIV) = DIV
    bytePrimitives(nme.MOD) = MOD
    bytePrimitives(nme.LT) = LT
    bytePrimitives(nme.LE) = LE
    bytePrimitives(nme.GT) = GT
    bytePrimitives(nme.GE) = GE
    bytePrimitives(nme.XOR) = XOR
    bytePrimitives(nme.OR) = OR
    bytePrimitives(nme.AND) = AND
    bytePrimitives(nme.LSL) = LSL
    bytePrimitives(nme.LSR) = LSR
    bytePrimitives(nme.ASR) = ASR
    // conversions
    bytePrimitives(nme.toByte) =   B2B
    bytePrimitives(nme.toShort) =  B2S
    bytePrimitives(nme.toChar) =   B2C
    bytePrimitives(nme.toInt) =    B2I
    bytePrimitives(nme.toLong) =   B2L
    bytePrimitives(nme.toFloat) =  B2F
    bytePrimitives(nme.toDouble) = B2D
    // unary methods
    bytePrimitives(nme.UNARY_+) = POS
    bytePrimitives(nme.UNARY_-) = NEG
    bytePrimitives(nme.UNARY_~) = NOT
    primitives(defn.ByteClass) = bytePrimitives

    // scala.Short
    val shortPrimitives = EqHashMap[Name, Int]()
    shortPrimitives(nme.EQ) = EQ
    shortPrimitives(nme.NE) = NE
    shortPrimitives(nme.ADD) = ADD
    shortPrimitives(nme.SUB) = SUB
    shortPrimitives(nme.MUL) = MUL
    shortPrimitives(nme.DIV) = DIV
    shortPrimitives(nme.MOD) = MOD
    shortPrimitives(nme.LT) = LT
    shortPrimitives(nme.LE) = LE
    shortPrimitives(nme.GT) = GT
    shortPrimitives(nme.GE) = GE
    shortPrimitives(nme.XOR) = XOR
    shortPrimitives(nme.OR) = OR
    shortPrimitives(nme.AND) = AND
    shortPrimitives(nme.LSL) = LSL
    shortPrimitives(nme.LSR) = LSR
    shortPrimitives(nme.ASR) = ASR
    // conversions
    shortPrimitives(nme.toByte) =   S2B
    shortPrimitives(nme.toShort) =  S2S
    shortPrimitives(nme.toChar) =   S2C
    shortPrimitives(nme.toInt) =    S2I
    shortPrimitives(nme.toLong) =   S2L
    shortPrimitives(nme.toFloat) =  S2F
    shortPrimitives(nme.toDouble) = S2D
    // unary methods
    shortPrimitives(nme.UNARY_+) = POS
    shortPrimitives(nme.UNARY_-) = NEG
    shortPrimitives(nme.UNARY_~) = NOT
    primitives(defn.ShortClass) = shortPrimitives

    // scala.Char
    val charPrimitives = EqHashMap[Name, Int]()
    charPrimitives(nme.EQ) = EQ
    charPrimitives(nme.NE) = NE
    charPrimitives(nme.ADD) = ADD
    charPrimitives(nme.SUB) = SUB
    charPrimitives(nme.MUL) = MUL
    charPrimitives(nme.DIV) = DIV
    charPrimitives(nme.MOD) = MOD
    charPrimitives(nme.LT) = LT
    charPrimitives(nme.LE) = LE
    charPrimitives(nme.GT) = GT
    charPrimitives(nme.GE) = GE
    charPrimitives(nme.XOR) = XOR
    charPrimitives(nme.OR) = OR
    charPrimitives(nme.AND) = AND
    charPrimitives(nme.LSL) = LSL
    charPrimitives(nme.LSR) = LSR
    charPrimitives(nme.ASR) = ASR
    // conversions
    charPrimitives(nme.toByte) =   C2B
    charPrimitives(nme.toShort) =  C2S
    charPrimitives(nme.toChar) =   C2C
    charPrimitives(nme.toInt) =    C2I
    charPrimitives(nme.toLong) =   C2L
    charPrimitives(nme.toFloat) =  C2F
    charPrimitives(nme.toDouble) = C2D
    // unary methods
    charPrimitives(nme.UNARY_+) = POS
    charPrimitives(nme.UNARY_-) = NEG
    charPrimitives(nme.UNARY_~) = NOT
    primitives(defn.CharClass) = charPrimitives

    // scala.Int
    val intPrimitives = EqHashMap[Name, Int]()
    intPrimitives(nme.EQ) = EQ
    intPrimitives(nme.NE) = NE
    intPrimitives(nme.ADD) = ADD
    intPrimitives(nme.SUB) = SUB
    intPrimitives(nme.MUL) = MUL
    intPrimitives(nme.DIV) = DIV
    intPrimitives(nme.MOD) = MOD
    intPrimitives(nme.LT) = LT
    intPrimitives(nme.LE) = LE
    intPrimitives(nme.GT) = GT
    intPrimitives(nme.GE) = GE
    intPrimitives(nme.XOR) = XOR
    intPrimitives(nme.OR) = OR
    intPrimitives(nme.AND) = AND
    intPrimitives(nme.LSL) = LSL
    intPrimitives(nme.LSR) = LSR
    intPrimitives(nme.ASR) = ASR
    // conversions
    intPrimitives(nme.toByte) =   I2B
    intPrimitives(nme.toShort) =  I2S
    intPrimitives(nme.toChar) =   I2C
    intPrimitives(nme.toInt) =    I2I
    intPrimitives(nme.toLong) =   I2L
    intPrimitives(nme.toFloat) =  I2F
    intPrimitives(nme.toDouble) = I2D
    // unary methods
    intPrimitives(nme.UNARY_+) = POS
    intPrimitives(nme.UNARY_-) = NEG
    intPrimitives(nme.UNARY_~) = NOT
    primitives(defn.IntClass) = intPrimitives

    // scala.Long
    val longPrimitives = EqHashMap[Name, Int]()
    longPrimitives(nme.EQ) = EQ
    longPrimitives(nme.NE) = NE
    longPrimitives(nme.ADD) = ADD
    longPrimitives(nme.SUB) = SUB
    longPrimitives(nme.MUL) = MUL
    longPrimitives(nme.DIV) = DIV
    longPrimitives(nme.MOD) = MOD
    longPrimitives(nme.LT) = LT
    longPrimitives(nme.LE) = LE
    longPrimitives(nme.GT) = GT
    longPrimitives(nme.GE) = GE
    longPrimitives(nme.XOR) = XOR
    longPrimitives(nme.OR) = OR
    longPrimitives(nme.AND) = AND
    longPrimitives(nme.LSL) = LSL
    longPrimitives(nme.LSR) = LSR
    longPrimitives(nme.ASR) = ASR
    // conversions
    longPrimitives(nme.toByte) =   L2B
    longPrimitives(nme.toShort) =  L2S
    longPrimitives(nme.toChar) =   L2C
    longPrimitives(nme.toInt) =    L2I
    longPrimitives(nme.toLong) =   L2L
    longPrimitives(nme.toFloat) =  L2F
    longPrimitives(nme.toDouble) = L2D
    // unary methods
    longPrimitives(nme.UNARY_+) = POS
    longPrimitives(nme.UNARY_-) = NEG
    longPrimitives(nme.UNARY_~) = NOT
    primitives(defn.LongClass) = longPrimitives

    // scala.Float
    val floatPrimitives = EqHashMap[Name, Int]()
    floatPrimitives(nme.EQ) = EQ
    floatPrimitives(nme.NE) = NE
    floatPrimitives(nme.ADD) = ADD
    floatPrimitives(nme.SUB) = SUB
    floatPrimitives(nme.MUL) = MUL
    floatPrimitives(nme.DIV) = DIV
    floatPrimitives(nme.MOD) = MOD
    floatPrimitives(nme.LT) = LT
    floatPrimitives(nme.LE) = LE
    floatPrimitives(nme.GT) = GT
    floatPrimitives(nme.GE) = GE
    // conversions
    floatPrimitives(nme.toByte) =   F2B
    floatPrimitives(nme.toShort) =  F2S
    floatPrimitives(nme.toChar) =   F2C
    floatPrimitives(nme.toInt) =    F2I
    floatPrimitives(nme.toLong) =   F2L
    floatPrimitives(nme.toFloat) =  F2F
    floatPrimitives(nme.toDouble) = F2D
    // unary methods
    floatPrimitives(nme.UNARY_+) = POS
    floatPrimitives(nme.UNARY_-) = NEG
    primitives(defn.FloatClass) = floatPrimitives

    // scala.Double
    val doublePrimitives = EqHashMap[Name, Int]()
    doublePrimitives(nme.EQ) = EQ
    doublePrimitives(nme.NE) = NE
    doublePrimitives(nme.ADD) = ADD
    doublePrimitives(nme.SUB) = SUB
    doublePrimitives(nme.MUL) = MUL
    doublePrimitives(nme.DIV) = DIV
    doublePrimitives(nme.MOD) = MOD
    doublePrimitives(nme.LT) = LT
    doublePrimitives(nme.LE) = LE
    doublePrimitives(nme.GT) = GT
    doublePrimitives(nme.GE) = GE
    // conversions
    doublePrimitives(nme.toByte) =   D2B
    doublePrimitives(nme.toShort) =  D2S
    doublePrimitives(nme.toChar) =   D2C
    doublePrimitives(nme.toInt) =    D2I
    doublePrimitives(nme.toLong) =   D2L
    doublePrimitives(nme.toFloat) =  D2F
    doublePrimitives(nme.toDouble) = D2D
    // unary methods
    doublePrimitives(nme.UNARY_+) = POS
    doublePrimitives(nme.UNARY_-) = NEG
    primitives(defn.DoubleClass) = doublePrimitives

    primitives
  }
}
