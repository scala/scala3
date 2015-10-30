object overloadedAccess {

  trait ST {
    def f(x: Object): Int = 1
    def f(x: Int): Unit = ()
  }

  object O extends ST {
    def f(x: String): Unit = ()
  }

  class C extends ST {
    import O._       // needs to pick inherited member because they are made visible in same scope.
    val x = f("abc")
    val y: Int = x
  }
}
