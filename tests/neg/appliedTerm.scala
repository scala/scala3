object SimpleEqs {
  val x = 1
  val y: {x} = x
  implicitly[{x + 1} =:= {y}]  // error
  implicitly[{x + 1} =:= {y + 2}]  // error
  implicitly[{x + 1} =:= {1 + y}]  // error: TypeComparer doesn't know about commutativity

  val b = true
  implicitly[{b} =:= {b}]
  implicitly[{!b} =:= {!b}]
  implicitly[{!b} =:= {b}]  // error
}


object Stability {
  def f1(x: Int): Int = x
  def f2(x: Int): {x} = x

  val x = 1
  implicitly[{f1(x)} =:= {x}]  // error: f1 is not considered stable  // error: f1's result type is not precise enough
  implicitly[{f1(x)} =:= {f1(x)}]  // error: f1 is not considered stable  // error: f1 is not considered stable
  implicitly[{f2(x)} =:= {x}]
  implicitly[{f2(x)} =:= {f2(x)}]
  implicitly[{f1(x)} =:= {f2(x)}]  // error: f1 is not considered stable  // error: f1's result type is not precise enough
}
