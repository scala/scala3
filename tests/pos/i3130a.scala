object O {
  new D(2).DD.apply()
}

 class D(val x: Int) {
  class DD()
  object DD {
   transparent def apply() = x // new DD()
  }
}
