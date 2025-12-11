import collection.mutable.ArrayBuffer
object Test {

  def main(args: Array[String]) = {
    val x = true
    val n = 1
    inline val nn = 2
    val y = if (x) 'A' else n
    val z: Int = y // error: no widening

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
    val zz2: Float = yy2 // error: no widening from Char to Float

    val yy3 = n match {
      case 1 => 'A'
      case 2 => 3L
      case 3 => 1.0f
    }
    val zz3: Double = yy3 // error: no widening from Char to Double

    val a = try {
      'A'
    } catch {
      case ex: Exception => nn
      case ex: Error => 3L
    }
    val b: Long = a // error: no widening

    val a_ok = try {
        1
      } catch {
        case ex: Exception => nn
        case ex: Error => 3L
      }

    val b_ok: Long = a_ok // ok

    val xs = List(1.0, nn, 'c')
    val ys: List[Double] = xs // error: no widening

  }

  def arraytest =
    inline val b = 33
    def f(): Int = b + 1
    val a1 = Array(b, 33, 'a')
    val b1: Array[Int] = a1    // OK, Array constructor selection uses weak conformance
    val a2 = Array(b, 33, 'a', f())
    val b2: Array[Int] = a2    // OK, Array constructor selection uses weak conformance
    val a3 = Array(1.0f, 'a', 0)
    val b3: Array[Float] = a3  // OK, Array constructor selection uses weak conformance
    val a4 = Array(1.0f, 1L)
    val b4: Array[Double] = a4 // error: Array[Float] is picked
    val a5 = Array(1.0f, 1L, f())
    val b5: Array[AnyVal] = a5 // error: Array[Float] is picked
    val a6 = Array(1.0f, 1234567890)
    val b6: Array[AnyVal] = a6 // error: Array[Float] is picked

  def arrayBufferTest =
    inline val b = 33
    def f(): Int = b + 1
    val a1 = ArrayBuffer(b, 33, 'a')
    val b1: ArrayBuffer[Int] = a1    // error: no widening
    val a2 = ArrayBuffer(b, 33, 'a', f())
    val b2: ArrayBuffer[Int] = a2    // error: no widening
    val a3 = ArrayBuffer(1.0f, 'a', 0)
    val b3: ArrayBuffer[Float] = a3  // error: no widening
    val a4 = ArrayBuffer(1.0f, 1L)
    val b4: ArrayBuffer[Double] = a4 // error: no widening
    val a5 = ArrayBuffer(1.0f, 1L, f())
    val b5: ArrayBuffer[Float | Long | Int] = a5 // error: no widening
    val a6 = ArrayBuffer(1.0f, 1234567890)
    val b6: ArrayBuffer[Float | Int] = a6 // error: no widening

  def totalDuration(results: List[Long], cond: Boolean): Long =
    results.map(r => if (cond) r else 0).sum
  def totalDuration2(results: List[Long], cond: Boolean): Long =
    results.map{ r =>
      cond match {
        case true => r
        case false => 0
      }
    }.sum
  def totalDuration3(results: List[Long], cond: Boolean): Long =
    results.map{ r =>
      try r
      catch { case ex: Exception => 0 }
    }.sum
}
