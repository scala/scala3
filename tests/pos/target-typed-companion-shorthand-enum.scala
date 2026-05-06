import scala.language.experimental.targetTypedCompanionShorthand

object EnumSupport:

  enum Color:
    case Red, Blue, Green

  enum Tree[+A]:
    case Leaf
    case Node(value: A, left: Tree[A], right: Tree[A])

  def show(c: Color): String = c match
    case .Red   => "R"
    case .Blue  => "B"
    case .Green => "G"

  val c1: Color = .Red
  val c2: Color = .Blue

  // Parameterised enum case (value-class and type-arg interaction).
  val t: Tree[Int] = .Leaf
  val t2: Tree[Int] = .Node(1, .Leaf, .Leaf)
