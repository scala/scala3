package example

class MethodUsages {
  val m = new Methods[Int]
  m.m1
  m.m2()
  m.m3(0)
  m.m4(0)(0)
  m.m5("")
  m.m5(0)
  m.m6(0)
  m.m6(new m.List[Int])
  m.m6(Nil)
  m.m7a(m, new m.List[Int])
  m.m7b(new m.List[Int])
  m.`m8().`()
  m.m9(null)
  m.m10(null)
  m.m11(Predef)
  m.m11(OExample)
  m.m12a(null)
  m.m12b(null)
  m.m13(0)
  m.m15(0)
  m.m16(0)
  m.m16(0)
  m.m17.m()
  m.m17(1)
  m.m17("")
  m.m18.m()
  m.m18(1)
  m.m18("")
}
