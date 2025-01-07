import language.experimental.captureChecking

trait Source[+T]

def race[T](@caps.use sources: (Source[T]^)*): Source[T]^{sources*} = ???

def raceTwo[T](src1: Source[T]^, src2: Source[T]^): Source[T]^{} =
  race(Seq(src1, src2)*) // error

def raceThree[T](src1: Source[T]^, src2: Source[T]^): Source[T]^{} =
  race(src1, src2) // error