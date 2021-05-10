enum Func[-A, +B] {
  case Double extends Func[Int, Int]
  case ToString extends Func[Float, String]

  def run: A => B = this match {
    case Double => (x: Int) => x * 2
    case ToString => (x: Float) => x.toString
  }
}
