inline trait A(val x: => Int)

class B extends A({throw new Exception; 1})

@main def Test: Unit =
  val b = B()
