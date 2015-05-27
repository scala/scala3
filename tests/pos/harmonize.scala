object Test {

  def main(args: Array[String]) = {
    val x = true
    val n = 1
    val y = if (x) 'A' else n
    val z: Int = y

    val yy = n match {
      case 1 => 'A'
      case 2 => n
      case 3 => 1.0
    }
    val zz: Double = yy

    val a = try {
      'A'
    } catch {
      case ex: Exception => n
      case ex: Error => 3L
    }
    val b: Long = a

    val xs = List(1.0, n, 'c')
    val ys: List[Double] = xs
  }

}
