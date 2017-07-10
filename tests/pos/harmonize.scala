object Test {

  def main(args: Array[String]) = {
    val x = true
    val n = 1
    inline val nn = 2
    val y = if (x) 'A' else n
    val z: Int = y

    val yy1 = n match {
      case 1 => 'A'
      case 2 => n
      case 3 => 1.0
    }
    val zz1: AnyVal = yy1 // no widening

    val yy2 = n match {
      case 1 => 'A'
      case 2 => nn
      case 3 => 1.0f
    }
    val zz2: Float = yy2 // widening to Float

    val yy3 = n match {
      case 1 => 'A'
      case 2 => 3L
      case 3 => 1.0f
    }
    val zz3: Double = yy3 // widening to Double

    val a = try {
      'A'
    } catch {
      case ex: Exception => nn
      case ex: Error => 3L
    }
    val b: Long = a

    val xs = List(1.0, nn, 'c')
    val ys: List[Double] = xs
  }

}
