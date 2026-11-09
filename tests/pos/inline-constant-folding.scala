

inline def f1() =
  inline if 2 + 2 == 4 then true else false

inline def f2() =
  inline if {2 + 2 == 4} then true else false

inline def f3() =
  inline if {2 + (2: Int) == 4} then true else false

inline def f4() =
  inline if {2 + (2: Byte) == 4} then true else false

@main def Test =
  f1()
  f2()
  f3()
  f4()
