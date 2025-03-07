abstract class Source[+T, Cap^]:
  def transformValuesWith[U](f: (T -> U)^{Cap^}): Source[U, Cap]^{this, f} = ???

// TODO: The extension version of `transformValuesWith` doesn't work currently.
// extension[T, Cap^](src: Source[T, Cap]^)
//   def transformValuesWith[U](f: (T -> U)^{Cap^}): Source[U, Cap]^{src, f} = ???

def race[T, Cap^](sources: Source[T, Cap]^{Cap^}*): Source[T, Cap]^{Cap^} = ???

def either[T1, T2, Cap^](
    src1: Source[T1, Cap]^{Cap^},
    src2: Source[T2, Cap]^{Cap^}): Source[Either[T1, T2], Cap]^{Cap^} =
  val left = src1.transformValuesWith(Left(_))
  val right = src2.transformValuesWith(Right(_))
  race[Either[T1, T2], Cap](left, right)
  // Explicit type arguments are required here because the second argument
  // is inferred as `CapSet^{Cap^}` instead of `Cap`.
  // Although `CapSet^{Cap^}` subsumes `Cap` in terms of capture sets,
  // `Cap` is not a subtype of `CapSet^{Cap^}` in terms of subtyping.
