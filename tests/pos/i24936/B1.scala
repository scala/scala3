// Additional test cases for issue #24936

package example2

// Test case: abstract class with self-type
abstract class B1 { self: P1.type =>
  import Inner.value
  def getValue: Int = value
}

// Test case: class referencing sibling object through self-type
trait B2 { self: P1.type =>
  def siblingAccess: String = P2.name
}

// Test case: generic trait with self-type
trait B3[T] { self: P1.type =>
  def genericAccess: T = Inner.genericValue.asInstanceOf[T]
}

object P1 extends B1 with B2 with B3[Int]:
  object Inner:
    val value: Int = 100
    val genericValue: Any = 42

object P2:
  val name: String = "P2"
