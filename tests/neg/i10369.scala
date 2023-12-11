type Upgrade[T] = T match
  case Int => Double
  case Char => String
  case Boolean => Boolean

val upgrade: [t] => t => Upgrade[t] = new PolyFunction: // error
  def apply[T](x: T): Upgrade[T] = x match
    case x: Int => x.toDouble
    case x: Char => x.toString
    case x: Boolean => !x
