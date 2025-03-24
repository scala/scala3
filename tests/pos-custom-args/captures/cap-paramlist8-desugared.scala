  import language.experimental.captureChecking
  import caps.cap

  trait Ctx[T >: Nothing <: Any]() extends Object

  def test: Unit =
    {
      val x: Any^{cap} = ???
      val y: Any^{cap} = ???
      object O {
        val z: Any^{cap} = ???
      }
      val baz3:
        Int -> [C >: caps.CapSet <: caps.CapSet^,
          D >: caps.CapSet <: caps.CapSet^{C^}, E >: caps.CapSet <:
          caps.CapSet^{C^, x}] => () -> [F >: caps.CapSet^{x, y} <:
          caps.CapSet^{C^, E^}] => (x: Int) -> (Ctx[F]) ?-> Int
        = (i: Int) => [
        C >: _root_.scala.caps.CapSet <: _root_.scala.caps.CapSet^{cap},
        D >: _root_.scala.caps.CapSet <: _root_.scala.caps.CapSet^{C^},
        E >: _root_.scala.caps.CapSet <: _root_.scala.caps.CapSet^{C^, x}] =>
        () => [
        F
            >: _root_.scala.caps.CapSet^{x, y} <:
            _root_.scala.caps.CapSet^{C^, E^}
        ] => (x: Int) => (ev: Ctx[F]) ?=> 1
      ()
    }
