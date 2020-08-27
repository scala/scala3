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

@main def Test =
  assert(Outer().thunk().toString == "A1")
  assert(Outer().thunk2()(0).toString == "A2")
  assert(Outer2.thunk().toString == "B1")
  assert(Outer2.thunk2()(0).toString == "B2")
