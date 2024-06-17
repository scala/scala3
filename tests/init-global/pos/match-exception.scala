class MyException(val x: Int) extends Exception

object O1 {
  def f(): Throwable = {
    try {
      Exception()
    }
    catch {
      case t: Throwable =>
        t
    }
  }


  val x = f()
}

object O2 {
  def g() = throw MyException(10)

  def f(i: Int): Int = {
    try {
      if (i > 0) then g() else throw Exception()
    }
    catch {
      case t: MyException => t.x
      case _ => 0
    }
  }

  val x = f(2)
}

object O3 {
  var x = 0
  val y = {
    try {
      if (x >= 0) then throw MyException(x)
      else try {
        throw MyException(x)
      } catch {
        case t: Throwable => throw t
      }
    } catch {
      case t: MyException => t.x
    }
  }
}
