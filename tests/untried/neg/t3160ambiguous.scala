object Bippy {
  private class List[+T]
}
class Bippy {
  import Bippy.*
  import scala.collection.immutable.*

  def f(x: List[Any]): String = ???  // ambiguous, because Bippy.List is accessible
}
class Other {
  import Bippy.*
  import scala.collection.immutable.*

  def f(x: List[Any]): String = ???  // unambiguous, because Bippy.List is inaccessible
}
