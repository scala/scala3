package dotty.tools
package backend

object ScalaPrimitivesOps extends ScalaPrimitivesOps

class ScalaPrimitivesOps {
  // Arithmetic unary operations
  final val POS = 1                            // +x
  final val NEG = 2                            // -x
  final val NOT = 3                            // ~x

  // Arithmetic binary operations
  final val ADD = 10                           // x + y
  final val SUB = 11                           // x - y
  final val MUL = 12                           // x * y
  final val DIV = 13                           // x / y
  final val MOD = 14                           // x % y

  // Bitwise operations
  final val OR  = 20                           // x | y
  final val XOR = 21                           // x ^ y
  final val AND = 22                           // x & y

  // Shift operations
  final val LSL = 30                           // x << y
  final val LSR = 31                           // x >>> y
  final val ASR = 32                           // x >> y

  // Comparison operations
  final val ID = 40                            // x eq y
  final val NI = 41                            // x ne y
  final val EQ = 42                            // x == y
  final val NE = 43                            // x != y
  final val LT = 44                            // x < y
  final val LE = 45                            // x <= y
  final val GE = 46                            // x > y
  final val GT = 47                            // x >= y

  // Boolean unary operations
  final val ZNOT = 50                          // !x

  // Boolean binary operations
  final val ZOR = 60                           // x || y
  final val ZAND = 61                          // x && y

  // Array operations
  final val LENGTH = 70                        // x.length
  final val APPLY  = 71                        // x(y)
  final val UPDATE = 72                        // x(y) = z

  // Any operations
  final val IS = 80                            // x.is[y]
  final val AS = 81                            // x.as[y]
  final val HASH = 87                          // x.##

  // AnyRef operations
  final val SYNCHRONIZED = 90                  // x.synchronized(y)

  // String operations
  final val CONCAT = 100                       // String.valueOf(x)+String.valueOf(y)

  // coercions
  final val COERCE = 101

  // RunTime operations
  final val BOX = 110                          // RunTime.box_<X>(x)
  final val UNBOX = 111                        // RunTime.unbox_<X>(x)
  final val NEW_ZARRAY = 112                   // RunTime.zarray(x)
  final val NEW_BARRAY = 113                   // RunTime.barray(x)
  final val NEW_SARRAY = 114                   // RunTime.sarray(x)
  final val NEW_CARRAY = 115                   // RunTime.carray(x)
  final val NEW_IARRAY = 116                   // RunTime.iarray(x)
  final val NEW_LARRAY = 117                   // RunTime.larray(x)
  final val NEW_FARRAY = 118                   // RunTime.farray(x)
  final val NEW_DARRAY = 119                   // RunTime.darray(x)
  final val NEW_OARRAY = 120                   // RunTime.oarray(x)

  final val ZARRAY_LENGTH = 131                // RunTime.zarray_length(x)
  final val BARRAY_LENGTH = 132                // RunTime.barray_length(x)
  final val SARRAY_LENGTH = 133                // RunTime.sarray_length(x)
  final val CARRAY_LENGTH = 134                // RunTime.carray_length(x)
  final val IARRAY_LENGTH = 135                // RunTime.iarray_length(x)
  final val LARRAY_LENGTH = 136                // RunTime.larray_length(x)
  final val FARRAY_LENGTH = 137                // RunTime.farray_length(x)
  final val DARRAY_LENGTH = 138                // RunTime.darray_length(x)
  final val OARRAY_LENGTH = 139                // RunTime.oarray_length(x)

  final val ZARRAY_GET = 140                   // RunTime.zarray_get(x,y)
  final val BARRAY_GET = 141                   // RunTime.barray_get(x,y)
  final val SARRAY_GET = 142                   // RunTime.sarray_get(x,y)
  final val CARRAY_GET = 143                   // RunTime.carray_get(x,y)
  final val IARRAY_GET = 144                   // RunTime.iarray_get(x,y)
  final val LARRAY_GET = 145                   // RunTime.larray_get(x,y)
  final val FARRAY_GET = 146                   // RunTime.farray_get(x,y)
  final val DARRAY_GET = 147                   // RunTime.darray_get(x,y)
  final val OARRAY_GET = 148                   // RunTime.oarray_get(x,y)

  final val ZARRAY_SET = 150                   // RunTime.zarray(x,y,z)
  final val BARRAY_SET = 151                   // RunTime.barray(x,y,z)
  final val SARRAY_SET = 152                   // RunTime.sarray(x,y,z)
  final val CARRAY_SET = 153                   // RunTime.carray(x,y,z)
  final val IARRAY_SET = 154                   // RunTime.iarray(x,y,z)
  final val LARRAY_SET = 155                   // RunTime.larray(x,y,z)
  final val FARRAY_SET = 156                   // RunTime.farray(x,y,z)
  final val DARRAY_SET = 157                   // RunTime.darray(x,y,z)
  final val OARRAY_SET = 158                   // RunTime.oarray(x,y,z)

  final val B2B = 200                          // RunTime.b2b(x)
  final val B2S = 201                          // RunTime.b2s(x)
  final val B2C = 202                          // RunTime.b2c(x)
  final val B2I = 203                          // RunTime.b2i(x)
  final val B2L = 204                          // RunTime.b2l(x)
  final val B2F = 205                          // RunTime.b2f(x)
  final val B2D = 206                          // RunTime.b2d(x)

  final val S2B = 210                          // RunTime.s2b(x)
  final val S2S = 211                          // RunTime.s2s(x)
  final val S2C = 212                          // RunTime.s2c(x)
  final val S2I = 213                          // RunTime.s2i(x)
  final val S2L = 214                          // RunTime.s2l(x)
  final val S2F = 215                          // RunTime.s2f(x)
  final val S2D = 216                          // RunTime.s2d(x)

  final val C2B = 220                          // RunTime.c2b(x)
  final val C2S = 221                          // RunTime.c2s(x)
  final val C2C = 222                          // RunTime.c2c(x)
  final val C2I = 223                          // RunTime.c2i(x)
  final val C2L = 224                          // RunTime.c2l(x)
  final val C2F = 225                          // RunTime.c2f(x)
  final val C2D = 226                          // RunTime.c2d(x)

  final val I2B = 230                          // RunTime.i2b(x)
  final val I2S = 231                          // RunTime.i2s(x)
  final val I2C = 232                          // RunTime.i2c(x)
  final val I2I = 233                          // RunTime.i2i(x)
  final val I2L = 234                          // RunTime.i2l(x)
  final val I2F = 235                          // RunTime.i2f(x)
  final val I2D = 236                          // RunTime.i2d(x)

  final val L2B = 240                          // RunTime.l2b(x)
  final val L2S = 241                          // RunTime.l2s(x)
  final val L2C = 242                          // RunTime.l2c(x)
  final val L2I = 243                          // RunTime.l2i(x)
  final val L2L = 244                          // RunTime.l2l(x)
  final val L2F = 245                          // RunTime.l2f(x)
  final val L2D = 246                          // RunTime.l2d(x)

  final val F2B = 250                          // RunTime.f2b(x)
  final val F2S = 251                          // RunTime.f2s(x)
  final val F2C = 252                          // RunTime.f2c(x)
  final val F2I = 253                          // RunTime.f2i(x)
  final val F2L = 254                          // RunTime.f2l(x)
  final val F2F = 255                          // RunTime.f2f(x)
  final val F2D = 256                          // RunTime.f2d(x)

  final val D2B = 260                          // RunTime.d2b(x)
  final val D2S = 261                          // RunTime.d2s(x)
  final val D2C = 262                          // RunTime.d2c(x)
  final val D2I = 263                          // RunTime.d2i(x)
  final val D2L = 264                          // RunTime.d2l(x)
  final val D2F = 265                          // RunTime.d2f(x)
  final val D2D = 266                          // RunTime.d2d(x)

  /** Check whether the given operation code is an array operation. */
  def isArrayOp(code: Int): Boolean =
    isArrayNew(code) | isArrayLength(code) | isArrayGet(code) | isArraySet(code)

  def isArrayNew(code: Int): Boolean = code match {
    case NEW_ZARRAY | NEW_BARRAY | NEW_SARRAY | NEW_CARRAY |
         NEW_IARRAY | NEW_LARRAY | NEW_FARRAY | NEW_DARRAY |
         NEW_OARRAY => true
    case _ => false
  }

  def isArrayLength(code: Int): Boolean = code match {
    case ZARRAY_LENGTH | BARRAY_LENGTH | SARRAY_LENGTH | CARRAY_LENGTH |
         IARRAY_LENGTH | LARRAY_LENGTH | FARRAY_LENGTH | DARRAY_LENGTH |
         OARRAY_LENGTH | LENGTH => true
    case _ => false
  }

  def isArrayGet(code: Int): Boolean = code match {
    case ZARRAY_GET | BARRAY_GET | SARRAY_GET | CARRAY_GET |
         IARRAY_GET | LARRAY_GET | FARRAY_GET | DARRAY_GET |
         OARRAY_GET | APPLY => true
    case _ => false
  }

  def isArraySet(code: Int): Boolean = code match {
    case ZARRAY_SET | BARRAY_SET | SARRAY_SET | CARRAY_SET |
         IARRAY_SET | LARRAY_SET | FARRAY_SET | DARRAY_SET |
         OARRAY_SET | UPDATE => true
    case _ => false
  }

  /** Check whether the given code is a comparison operator */
  def isComparisonOp(code: Int): Boolean = code match {
    case ID | NI | EQ | NE |
         LT | LE | GT | GE => true

    case _ => false
  }
  def isUniversalEqualityOp(code: Int): Boolean = (code == EQ) || (code == NE)
  def isReferenceEqualityOp(code: Int): Boolean = (code == ID) || (code == NI)

  def isArithmeticOp(code: Int): Boolean = code match {
    case POS | NEG | NOT => true; // unary
    case ADD | SUB | MUL |
         DIV | MOD       => true; // binary
    case OR  | XOR | AND |
         LSL | LSR | ASR => true; // bitwise
    case _ => false
  }

  def isLogicalOp(code: Int): Boolean = code match {
    case ZNOT | ZAND | ZOR => true
    case _ => false
  }

  def isShiftOp(code: Int): Boolean = code match {
    case LSL | LSR | ASR => true
    case _ => false
  }

  def isBitwiseOp(code: Int): Boolean = code match {
    case OR | XOR | AND => true
    case _ => false
  }

  def isCoercion(code: Int): Boolean = (code >= B2B) && (code <= D2D)

}
