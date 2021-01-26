trait IntToLong:
  def apply(v: Int) : Long

inline def convert1(       f: IntToLong) = ???
inline def convert2(inline f: IntToLong) = ???

val test1 = convert1(_ * 10)
val test2 = convert2(_ * 10)
