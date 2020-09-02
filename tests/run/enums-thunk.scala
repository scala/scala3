class Outer {
  val thunk = { () =>
    enum E { case A1 }
    E.A1
  }
  val thunk2 = { () =>
    enum E { case A2 }
    E.values
  }
}

object Outer2 {
  val thunk = { () =>
    enum E { case B1 }
    E.B1
  }
  val thunk2 = { () =>
    enum E { case B2 }
    E.values
  }
}

object Outer3 {
  def thunk() = {
    enum E { case C1 }
    E.C1
  }
  def thunk2() = {
    enum E { case C2 }
    E.values
  }
}


@main def Test =
  assert(Outer().thunk().toString == "A1")
  assert(Outer().thunk2()(0).toString == "A2")
  assert(Outer2.thunk().toString == "B1")
  assert(Outer2.thunk2()(0).toString == "B2")
  assert(Outer3.thunk().toString == "C1")
  assert(Outer3.thunk2()(0).toString == "C2")
