package dotty.tools
package backend

object ScalaPrimitivesOps extends ScalaPrimitivesOps

class ScalaPrimitivesOps {
  // Arithmetic unary operations
  inline val POS = 1                            // +x
  inline val NEG = 2                            // -x
  inline val NOT = 3                            // ~x

  // Arithmetic binary operations
  inline val ADD = 10                           // x + y
  inline val SUB = 11                           // x - y
  inline val MUL = 12                           // x * y
  inline val DIV = 13                           // x / y
  inline val MOD = 14                           // x % y

  // Bitwise operations
  inline val OR  = 20                           // x | y
  inline val XOR = 21                           // x ^ y
  inline val AND = 22                           // x & y

  // Shift operations
  inline val LSL = 30                           // x << y
  inline val LSR = 31                           // x >>> y
  inline val ASR = 32                           // x >> y

  // Comparison operations
  inline val ID = 40                            // x eq y
  inline val NI = 41                            // x ne y
  inline val EQ = 42                            // x == y
  inline val NE = 43                            // x != y
  inline val LT = 44                            // x < y
  inline val LE = 45                            // x <= y
  inline val GT = 46                            // x > y
  inline val GE = 47                            // x >= y

  // Boolean unary operations
  inline val ZNOT = 50                          // !x

  // Boolean binary operations
  inline val ZOR = 60                           // x || y
  inline val ZAND = 61                          // x && y

  // Array operations
  inline val LENGTH = 70                        // x.length
  inline val APPLY  = 71                        // x(y)
  inline val UPDATE = 72                        // x(y) = z

  // Any operations
  inline val IS = 80                            // x.is[y]
  inline val AS = 81                            // x.as[y]
  inline val HASH = 87                          // x.##

  // AnyRef operations
  inline val SYNCHRONIZED = 90                  // x.synchronized(y)

  // String operations
  inline val CONCAT = 100                       // String.valueOf(x)+String.valueOf(y)

  // coercions
  inline val COERCE = 101

  // RunTime operations
  inline val BOX = 110                          // RunTime.box_<X>(x)
  inline val UNBOX = 111                        // RunTime.unbox_<X>(x)
  inline val NEW_ZARRAY = 112                   // RunTime.zarray(x)
  inline val NEW_BARRAY = 113                   // RunTime.barray(x)
  inline val NEW_SARRAY = 114                   // RunTime.sarray(x)
  inline val NEW_CARRAY = 115                   // RunTime.carray(x)
  inline val NEW_IARRAY = 116                   // RunTime.iarray(x)
  inline val NEW_LARRAY = 117                   // RunTime.larray(x)
  inline val NEW_FARRAY = 118                   // RunTime.farray(x)
  inline val NEW_DARRAY = 119                   // RunTime.darray(x)
  inline val NEW_OARRAY = 120                   // RunTime.oarray(x)

  inline val ZARRAY_LENGTH = 131                // RunTime.zarray_length(x)
  inline val BARRAY_LENGTH = 132                // RunTime.barray_length(x)
  inline val SARRAY_LENGTH = 133                // RunTime.sarray_length(x)
  inline val CARRAY_LENGTH = 134                // RunTime.carray_length(x)
  inline val IARRAY_LENGTH = 135                // RunTime.iarray_length(x)
  inline val LARRAY_LENGTH = 136                // RunTime.larray_length(x)
  inline val FARRAY_LENGTH = 137                // RunTime.farray_length(x)
  inline val DARRAY_LENGTH = 138                // RunTime.darray_length(x)
  inline val OARRAY_LENGTH = 139                // RunTime.oarray_length(x)

  inline val ZARRAY_GET = 140                   // RunTime.zarray_get(x,y)
  inline val BARRAY_GET = 141                   // RunTime.barray_get(x,y)
  inline val SARRAY_GET = 142                   // RunTime.sarray_get(x,y)
  inline val CARRAY_GET = 143                   // RunTime.carray_get(x,y)
  inline val IARRAY_GET = 144                   // RunTime.iarray_get(x,y)
  inline val LARRAY_GET = 145                   // RunTime.larray_get(x,y)
  inline val FARRAY_GET = 146                   // RunTime.farray_get(x,y)
  inline val DARRAY_GET = 147                   // RunTime.darray_get(x,y)
  inline val OARRAY_GET = 148                   // RunTime.oarray_get(x,y)

  inline val ZARRAY_SET = 150                   // RunTime.zarray(x,y,z)
  inline val BARRAY_SET = 151                   // RunTime.barray(x,y,z)
  inline val SARRAY_SET = 152                   // RunTime.sarray(x,y,z)
  inline val CARRAY_SET = 153                   // RunTime.carray(x,y,z)
  inline val IARRAY_SET = 154                   // RunTime.iarray(x,y,z)
  inline val LARRAY_SET = 155                   // RunTime.larray(x,y,z)
  inline val FARRAY_SET = 156                   // RunTime.farray(x,y,z)
  inline val DARRAY_SET = 157                   // RunTime.darray(x,y,z)
  inline val OARRAY_SET = 158                   // RunTime.oarray(x,y,z)

  inline val B2B = 200                          // RunTime.b2b(x)
  inline val B2S = 201                          // RunTime.b2s(x)
  inline val B2C = 202                          // RunTime.b2c(x)
  inline val B2I = 203                          // RunTime.b2i(x)
  inline val B2L = 204                          // RunTime.b2l(x)
  inline val B2F = 205                          // RunTime.b2f(x)
  inline val B2D = 206                          // RunTime.b2d(x)

  inline val S2B = 210                          // RunTime.s2b(x)
  inline val S2S = 211                          // RunTime.s2s(x)
  inline val S2C = 212                          // RunTime.s2c(x)
  inline val S2I = 213                          // RunTime.s2i(x)
  inline val S2L = 214                          // RunTime.s2l(x)
  inline val S2F = 215                          // RunTime.s2f(x)
  inline val S2D = 216                          // RunTime.s2d(x)

  inline val C2B = 220                          // RunTime.c2b(x)
  inline val C2S = 221                          // RunTime.c2s(x)
  inline val C2C = 222                          // RunTime.c2c(x)
  inline val C2I = 223                          // RunTime.c2i(x)
  inline val C2L = 224                          // RunTime.c2l(x)
  inline val C2F = 225                          // RunTime.c2f(x)
  inline val C2D = 226                          // RunTime.c2d(x)

  inline val I2B = 230                          // RunTime.i2b(x)
  inline val I2S = 231                          // RunTime.i2s(x)
  inline val I2C = 232                          // RunTime.i2c(x)
  inline val I2I = 233                          // RunTime.i2i(x)
  inline val I2L = 234                          // RunTime.i2l(x)
  inline val I2F = 235                          // RunTime.i2f(x)
  inline val I2D = 236                          // RunTime.i2d(x)

  inline val L2B = 240                          // RunTime.l2b(x)
  inline val L2S = 241                          // RunTime.l2s(x)
  inline val L2C = 242                          // RunTime.l2c(x)
  inline val L2I = 243                          // RunTime.l2i(x)
  inline val L2L = 244                          // RunTime.l2l(x)
  inline val L2F = 245                          // RunTime.l2f(x)
  inline val L2D = 246                          // RunTime.l2d(x)

  inline val F2B = 250                          // RunTime.f2b(x)
  inline val F2S = 251                          // RunTime.f2s(x)
  inline val F2C = 252                          // RunTime.f2c(x)
  inline val F2I = 253                          // RunTime.f2i(x)
  inline val F2L = 254                          // RunTime.f2l(x)
  inline val F2F = 255                          // RunTime.f2f(x)
  inline val F2D = 256                          // RunTime.f2d(x)

  inline val D2B = 260                          // RunTime.d2b(x)
  inline val D2S = 261                          // RunTime.d2s(x)
  inline val D2C = 262                          // RunTime.d2c(x)
  inline val D2I = 263                          // RunTime.d2i(x)
  inline val D2L = 264                          // RunTime.d2l(x)
  inline val D2F = 265                          // RunTime.d2f(x)
  inline val D2D = 266                          // RunTime.d2d(x)

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
