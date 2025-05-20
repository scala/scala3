import language.experimental.captureChecking
import caps.*

abstract class Source[+T, Cap^]:
  def transformValuesWith[U](f: (T -> U)^{Cap^}): Source[U, Cap]^{this, f} = ???

def race[T, Cap^](sources: Source[T, Cap]^{Cap^}*): Source[T, Cap]^{Cap^} = ???

def either[T1, T2, Cap^](
    //io: () => Unit,
    src1: Source[T1, Cap]^{Cap^},
    src2: Source[T2, Cap]^{Cap^}): Source[Either[T1, T2], Cap]^{Cap^} =
  val left = src1.transformValuesWith(Left(_))
  val right = src2.transformValuesWith(Right(_))
  race[Either[T1, T2], CapSet^{Cap^}](left, right) // <- rejected
