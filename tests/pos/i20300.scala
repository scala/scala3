trait T:

  def v() = ()

  trait F:
    def f(): Unit =
      inline def op(): Unit = v()
      op()