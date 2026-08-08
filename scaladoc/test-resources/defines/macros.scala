package pkg

// Issue #10823

/** @define macro Super */
abstract class Super {
  /** Super.inherited: $macro */
  def inherited: Int = 5
  /** Super.implemented: $macro */
  def implemented: Unit
  /** Super.overridden: $macro */
  def overridden: String = "test"
}
/**
 * @define name default
 * @define dummy dummy
 */
trait A[T] {
  /** List $name */
  def list(): List[T]
  /** Gets the $name with the given $dummy */
  def get(id: String): Option[T] = None
}
/** @define dummy id */
trait B extends A[String]
/**
 * @define macro Sub
 * @define name banana
 */
class Sub extends Super with B {
  def list(): List[String] = List.empty
  def implemented: Unit = ()
  override def overridden: String = "overridden test"
}