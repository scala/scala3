// test synthesizeSAMFunction where the sam type is not fully defined
class T {
  trait F[T, U] { def apply(x: T): U }
  // this is an inner trait, that will recieve an abstract $outer pointer. Not a SAM.
  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
}
