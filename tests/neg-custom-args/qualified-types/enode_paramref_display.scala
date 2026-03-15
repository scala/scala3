def test: Unit =
  val x: Int = ???

  // Simple: one lambda parameter clashes with a local variable named x
  val v1: {v: Int with v == x + 1} = x + 2 // error

  // Nested lambdas: two lambda parameters with different underlying types
  val v2: {l: List[Int] with l.forall(e => e > x)} = ??? : {l: List[Int] with l.nonEmpty && l.forall(e => e > x + 1)} // error

  // Nested lambdas: inner and outer params have the same underlying type (Int)
  def id(x: Int): Int = x
  val v3: {v: Int with v == id(x)} = x + 1 // error

  // Multi-param lambda
  def toBool[T](x: T): Boolean = ???
  val v4: {v: Int with toBool((a: Int, b: Int) => a < b)} = ??? : {v: Int with toBool((a: Int, b: Int) => a > b)} // error

  // Outer param referenced at different depths (l appears both outside and inside forall's lambda)
  val v5: {l: List[Int] with l.nonEmpty && l.forall(e => e > 0)} = ??? : {l: List[Int] with l.forall(e => e > 1)} // error
