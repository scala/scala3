inline def convFail[Of, From](from : From) : Unit = // removed inline from parameter to avoid unsound path selection
  val c = compiletime.summonInline[Conversion[from.type, Of]]

inline def convOK[Of, From](from : From)(using c : Conversion[from.type, Of]) : Unit = {} // removed inline from parameter to avoid unsound path selection

class Bar[T](value : T)
given [T <: Int] : Conversion[T, Bar[T]] = Bar(_)

@main def main : Unit = {
  convOK[Bar[1],1](1)
  convFail[Bar[1],1](1) //causes error
}
