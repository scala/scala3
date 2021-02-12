// test synthesizeSAMFunction where the sam type is not fully defined

trait F1[T, U] { def apply(x: T): U }
class T {
  import T.*
  // NOTE: the f(x) desugaring for now assumes the single abstract method is called 'apply'
  def app1[T, U](x: T)(f: F1[T, U]): U = f(x)
  def app2[T, U](x: T)(f: F2[T, U]): U = f(x)
  app1(1)(x => List(x))
  app2(1)(x => List(x))
}
object T{
  trait F2[T, U] { def apply(x: T): U }
}
