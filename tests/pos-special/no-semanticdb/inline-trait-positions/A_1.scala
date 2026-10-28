//> using options -language:experimental.inlineTraits

// Compiled without -Xsemanticdb (which turns on reading positions from TASTy): the body
// of `A` is read from TASTy and inlined into `B`, so it must be read with positions
// (-Ycheck:all reported "wrong source set" for the expanded macro call).
import scala.quoted.*

inline def three(): Int = ${ threeImpl }
def threeImpl(using Quotes): Expr[Int] = '{ 3 }

inline trait A:
  val i: Int = three()
