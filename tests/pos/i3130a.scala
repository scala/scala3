object O {
  new D(2).DD.apply()
}

 class D(val x: Int) {
  class DD()
  object DD {
   rewrite def apply() = x // new DD()
  }
}
