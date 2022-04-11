
class Test {
  implicit def compareComparables[T](x: T)(implicit ord: Ordering[T]) = // error: result type of implicit definition needs to be given explicitly
    new ord.OrderingOps(x)
  class Bippy { def compare(y: Bippy) = util.Random }
  () < () // error: value `<` is not a member of Unit
}
