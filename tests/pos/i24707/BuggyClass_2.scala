// Test for issue #24707: Compiler crash with implicit conversion, value class, and property assignment
// The compiler crashed when assigning to a field on the result of an implicit conversion
// because the LHS of the assignment was a Block (from the implicit conversion) wrapping a Select.

import scala.language.implicitConversions

// Value class that wraps MockComposite
final class RichComposite(val comp: MockComposite) extends AnyVal {
  def setRowLayout(margin: Int = 0, spacing: Int = 3,
                   vertical: Boolean = false): MockLayout = {
    val layout = new MockLayout(
      if (vertical) MockSWT.VERTICAL else MockSWT.HORIZONTAL)
    layout.marginTop = margin
    layout.marginBottom = margin
    layout.marginLeft = margin
    layout.marginRight = margin
    layout.spacing = spacing
    comp.setLayout(layout)
    layout
  }
}

object Implicits {
  implicit def toRichComposite(c: MockComposite): RichComposite =
    new RichComposite(c)
}

import Implicits._

// Original failing case
private final class BuggyClass(parent: MockComposite) {
  private val comp = new MockComposite(parent, MockSWT.NONE)
  locally {
    comp.setRowLayout(5, 5).wrap = false  // This was crashing
  }
}

// Additional test cases for field assignment through implicit conversion

// Test case: Multiple field assignments on same object
class TestMultipleAssignments(parent: MockComposite) {
  val comp = new MockComposite(parent, MockSWT.NONE)
  locally {
    val layout = comp.setRowLayout(0, 0)
    layout.wrap = true
    layout.spacing = 10
    layout.marginTop = 5
  }
}

// Test case: Chained field assignment
class TestChainedAssignment(parent: MockComposite) {
  val comp = new MockComposite(parent, MockSWT.NONE)
  locally {
    // Assignment on result of implicit conversion + method call
    comp.setRowLayout().wrap = true
    comp.setRowLayout(1).spacing = 2
    comp.setRowLayout(1, 2).marginTop = 3
    comp.setRowLayout(1, 2, true).marginBottom = 4
  }
}

// Test case: Non-value class implicit conversion
class RichMockLayout(val layout: MockLayout) {
  def setWrap(w: Boolean): MockLayout = {
    layout.wrap = w
    layout
  }
}

object MoreImplicits {
  implicit def toRichLayout(l: MockLayout): RichMockLayout =
    new RichMockLayout(l)
}

class TestNonValueClassImplicit(parent: MockComposite) {
  import MoreImplicits._
  val comp = new MockComposite(parent, MockSWT.NONE)
  locally {
    val layout = comp.setRowLayout(5, 5)
    layout.setWrap(true).spacing = 20  // Chained through non-value class implicit
  }
}

// Test case: Nested implicit conversions
class TestNestedImplicits(parent: MockComposite) {
  import MoreImplicits._
  val comp = new MockComposite(parent, MockSWT.NONE)
  locally {
    // First implicit conversion (MockComposite -> RichComposite)
    // Then method call returns MockLayout
    // Then field assignment
    comp.setRowLayout(5, 5).wrap = false

    // Implicit on result (MockLayout -> RichMockLayout), then field assignment
    comp.setRowLayout(5, 5).setWrap(true).marginTop = 10
  }
}

// Test case: Assignment in different contexts
class TestAssignmentContexts(parent: MockComposite) {
  val comp = new MockComposite(parent, MockSWT.NONE)

  // In method body
  def inMethod(): Unit = {
    comp.setRowLayout(1, 1).wrap = true
  }

  // In val initializer
  val x: Unit = {
    comp.setRowLayout(2, 2).wrap = false
    ()
  }

  // In if expression
  def inIf(cond: Boolean): Unit = {
    if (cond) {
      comp.setRowLayout(3, 3).wrap = true
    }
  }

  // In match expression
  def inMatch(opt: Option[Int]): Unit = {
    opt match {
      case Some(_) => comp.setRowLayout(4, 4).wrap = true
      case None => comp.setRowLayout(4, 4).wrap = false
    }
  }
}
