import language.experimental.captureChecking

trait Source[+T]

def race[T](sources: Seq[Source[T]^ @caps.use]): Source[T]^{sources*} = ???

def raceTwo[T](src1: Source[T]^, src2: Source[T]^): Source[T]^{}
  = race(Seq(src1, src2)) // error
    // this compiled and returned a Source that does not capture src1 and src2.