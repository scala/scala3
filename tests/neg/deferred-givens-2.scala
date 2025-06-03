//> using options -language:experimental.modularity -source future
trait Ord:
  type Self

trait Sorted:
  type Element: Ord

object Scoped:
  given (Int is Ord)()
  class SortedIntCorrect extends Sorted:
    type Element = Int

class SortedIntCorrect2 extends Sorted:
  type Element = Int
  override given given_Ord_Element: (Int is Ord)()

class SortedIntWrong1 extends Sorted: // error
  type Element = Int
  override given (Element is Ord)()

class SortedIntWrong2 extends Sorted: // error
  type Element = Int
  override given (Int is Ord)()