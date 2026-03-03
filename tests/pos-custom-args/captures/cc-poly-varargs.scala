import language.experimental.captureChecking
import language.experimental.separationChecking
abstract class Source[+T]

extension[T](src: Source[T]^)
  def transformValuesWith[U](f: (T -> U)^{src, caps.any}): Source[U]^{src, f} = ???

def race[T, D^](sources: Source[T]^{D}*): Source[T]^{D} = ???

def either[T1, T2](
    src1: Source[T1]^,
    src2: Source[T2]^): Source[Either[T1, T2]]^{src1, src2} =
  val left = src1.transformValuesWith(Left(_))    // ok
  val right = src2.transformValuesWith(Right(_))  // ok
  race(left, right)