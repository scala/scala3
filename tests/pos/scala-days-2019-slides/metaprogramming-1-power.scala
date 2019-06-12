object App2 {

  inline def power(x: Long, n: Int): Long = {
    if (n == 0)
      1L
    else if (n % 2 == 1)
      x * power(x, n - 1)
    else {
      val y: Long = x * x
      power(y, n / 2)
    }
  }

  val x: Long = 5L

  power(x, 10)

//  def badPower(x: Long, n: Int): Long = {
//    power(x, n) // error:  Maximal number of successive inlines (32) exceeded, Maybe this is caused by a recursive inline method?
//  }

}

object App3 {

  inline def power(x: Long, inline n: Int): Long = {
    if (n == 0)
      1L
    else if (n % 2 == 1)
      x * power(x, n - 1)
    else {
      val y: Long = x * x
      power(y, n / 2)
    }
  }

  val x: Long = 5L
  val n: Int = 10

  power(x, 10)

//  def badPower(x: Long, n: Int): Long = {
//    power(x, n) // error: argument to inline parameter must be a known value
//  }

}


object App4 {

  inline def power(x: Long, n: Int): Long = {
    inline if (n == 0)
      1L
    else inline if (n % 2 == 1)
      x * power(x, n - 1)
    else {
      val y: Long = x * x
      power(y, n / 2)
    }
  }

  val x: Long = 5L
  val n: Int = 10

  power(x, 10)

//  def badPower(x: Long, n: Int): Long = {
//    power(x, n) // error: cannot reduce inline if
//  }

}

