import caps.use

def test(io: Object^, async: Object^) =

  trait A:
    def f(@use x: List[() ->{io} Unit]): Unit

  class B extends A:
    def f(@use x: List[() => Unit]): Unit =  // error, would be unsound if allowed
      x.foreach(_())

  class C extends A:
    def f(@use x: List[() ->{io, async} Unit]): Unit =  // error, this one could be soundly allowed actually
      x.foreach(_())

