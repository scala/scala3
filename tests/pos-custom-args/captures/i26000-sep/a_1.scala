import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

// These definitions are compiled separately from their use in b_2.scala,
// so the capture sets and bounds referring to `C` in their inferred types
// must survive the TASTy round trip.
val fBoxed = Box([C^] => (x: File^{C}) => x)
val io: File^ = File()
val bounded = Box([C^ <: {io}] => (xs: List[File^{C}]) => xs)
