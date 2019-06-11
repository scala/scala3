// Ensure we omit valid JVM generic signatures when upper bounds are primitive
// types. Java generic signatures cannot use primitive types in that position.

class AUnit[B[_] <: Unit]
class ABoolean[B[_] <: Boolean]

class AByte[B[_] <: Byte]
class AChar[B[_] <: Char]

class AShort[B[_] <: Short]
class AInt[B[_] <: Int]
class ALong[B[_] <: Long]

class AFloat[B[_] <: Float]
class ADouble[B[_] <: Double]

class ValClU(val i: Unit) extends AnyVal
class AValClU[B[_] <: ValClU]
class ValClI(val i: Int) extends AnyVal
class AValClI[B[_] <: ValClI]

package object opaquetypes {
  opaque type Logarithm = Double
  object Logarithm {
    class ALogarithm[B[_] <: Logarithm]
    type ALog = ALogarithm[[x] =>> Logarithm]
    type ALogD = ALogarithm[[x] =>> Double]
    type ALogN = ALogarithm[[x] =>> Nothing]

    type A1Log = opaquetypes.ALogarithm[[x] =>> Logarithm]
    // type A1LogD = opaquetypes.ALogarithm[[x] =>> Double]
    type A1LogN = opaquetypes.ALogarithm[[x] =>> Nothing]

    class ALogNested[B[_] <: Logarithm]
    class ValClLogNested(val i: Logarithm) extends AnyVal
    class AValClLogNested[B[_] <: ValClLogNested]

    type A = AValClLogNested[[x] =>> ValClLogNested]
    type AN = AValClLogNested[[x] =>> Nothing]
  }
  class ValClLog(val i: Logarithm) extends AnyVal
  class AValClLog[B[_] <: ValClLog]
  type AClLog = AValClLog[[x] =>> ValClLog]
  type AClLogN = AValClLog[[x] =>> Nothing]

  class ADouble[B[_] <: Double]
  type AD = ADouble[[x] =>> Double]
  type ADN = ADouble[[x] =>> Nothing]

  class ALogarithm[B[_] <: Logarithm]
  type ALog = ALogarithm[[x] =>> Logarithm]
  type ALogN = ALogarithm[[x] =>> Nothing]

  type A1Log = Logarithm.ALogarithm[[x] =>> Logarithm]
  type A1LogN = Logarithm.ALogarithm[[x] =>> Nothing]
}

object Main {
  type AU = AUnit[[x] =>> Unit]
  type AUN = AUnit[[x] =>> Nothing]

  type AB = ABoolean[[x] =>> Boolean]
  type ABN = ABoolean[[x] =>> Nothing]


  type ABy = AByte[[x] =>> Byte]
  type AByN = AByte[[x] =>> Nothing]

  type AC = AChar[[x] =>> Char]
  type ACN = AChar[[x] =>> Nothing]


  type AS = AShort[[x] =>> Short]
  type ASN = AShort[[x] =>> Nothing]

  type AI = AInt[[x] =>> Int]
  type AIN = AInt[[x] =>> Nothing]

  type AL = ALong[[x] =>> Long]
  type ALN = ALong[[x] =>> Nothing]


  type AF = AFloat[[x] =>> Float]
  type AFN = AFloat[[x] =>> Nothing]

  type AD = ADouble[[x] =>> Double]
  type ADN = ADouble[[x] =>> Nothing]


  type ACU = AValClU[[x] =>> ValClU]
  type ACUN = AValClU[[x] =>> Nothing]

  type ACI = AValClI[[x] =>> ValClI]
  type ACIN = AValClI[[x] =>> Nothing]
}
