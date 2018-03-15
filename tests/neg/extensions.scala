import Predef.{any2stringadd => _, _}
object extensions {

// Simple extension methods

  case class Circle(x: Double, y: Double, radius: Double)

  extension CircleOps for Circle {
    def circumference = this.radius * math.Pi * 2
    private val p = math.Pi       // error: `def` expected
  }

// Trait implementations

  trait HasArea {
    def area: Double
  }

  abstract class HasAreaClass extends HasArea

  extension Ops2 : HasArea {} // error: `def` expected
  extension Ops for Circle extends HasArea {} // error: `def` expected

  extension Circle2 : HasArea { // error: `for` expected
    def area = this.radius * this.radius * math.Pi
  }

  extension Ops3 for Circle : HasAreaClass { // error: class HasAreaClass is not a trait
    def area = this.radius * this.radius * math.Pi
  }

// Generic trait implementations

  extension ListOps[T] for List[T] {
    type I = Int                 // error: `def` expected
    def second = this.tail.head
  }

// Specific trait implementations

  extension ListOps2 for List[Int] { self => // error: `def` expected
    import java.lang._ // error: `def` expected
    def maxx = (0 /: this)(_ `max` _)
  }
}
