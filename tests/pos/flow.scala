trait FlowOps[+Out] {
  type Repr[+O] <: FlowOps[O]
}

trait Flow[-In, +Out] extends FlowOps[Out] {
  override type Repr[+O] <: Flow[In, O]
  def map[T](f: Out => T): Repr[T] /* workaround: expand alias Flow[In, T] */
}

class Test {
  def slowFlow: Unit = {
    (null: Flow[String, String])
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b)
      .map(b => b) // takes an age to compile
  }
}
