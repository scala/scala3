object Test extends App {
  val f2 = (x1: Int, x2: Int) => x2
  assert(f2.curried(1)(2) == 2)

  val f3 = (x1: Int, x2: Int, x3: Int) => x3
  assert(f3.curried(1)(2)(3) == 3)

  val f4 = (x1: Int, x2: Int, x3: Int, x4: Int) => x4
  assert(f4.curried(1)(2)(3)(4) == 4)

  val f5 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int) => x5
  assert(f5.curried(1)(2)(3)(4)(5) == 5)

  val f6 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int) => x6
  assert(f6.curried(1)(2)(3)(4)(5)(6) == 6)

  val f7 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int) => x7
  assert(f7.curried(1)(2)(3)(4)(5)(6)(7) == 7)

  val f8 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int) => x8
  assert(f8.curried(1)(2)(3)(4)(5)(6)(7)(8) == 8)

  val f9 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int) => x9
  assert(f9.curried(1)(2)(3)(4)(5)(6)(7)(8)(9) == 9)

  val f10 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int) => x10
  assert(f10.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10) == 10)

  val f11 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int) => x11
  assert(f11.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11) == 11)

  val f12 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int) => x12
  assert(f12.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12) == 12)

  val f13 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int) => x13
  assert(f13.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13) == 13)

  val f14 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int) => x14
  assert(f14.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14) == 14)

  val f15 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int) => x15
  assert(f15.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15) == 15)

  val f16 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int) => x16
  assert(f16.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16) == 16)

  val f17 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int) => x17
  assert(f17.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17) == 17)

  val f18 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int) => x18
  assert(f18.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17)(18) == 18)

  val f19 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int) => x19
  assert(f19.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17)(18)(19) == 19)

  val f20 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int) => x20
  assert(f20.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17)(18)(19)(20) == 20)

  val f21 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int) => x21
  assert(f21.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17)(18)(19)(20)(21) == 21)

  val f22 = (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int) => x22
  assert(f22.curried(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17)(18)(19)(20)(21)(22) == 22)
}