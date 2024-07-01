object MyException extends Exception {
  var x: Int = 0
  def writeX(newx: Int) = x = newx // warn
}

object DangerousException extends Exception {
  var x = O3.y
}

object O1 {
  def f(): Int = {
    try {
      throw MyException
    }
    catch {
      case MyException =>
        MyException.writeX(x) // warn
        0
    }
  }


  val x = f()
}

object O2 {
  def g() = throw MyException

  def f(i: Int): Int = {
    try {
      if (i > 0) then g() else throw Exception()
    }
    catch {
      case MyException => 0
      case _ =>
        MyException.writeX(x) // warn
        0
    }
  }

  val x = f(2)
}

object O3 {
  var x = 0
  val y: Int = {
    try {
      if (x >= 0) then throw MyException
      else try {
        throw MyException
      } catch {
        case MyException => throw MyException
        case t: DangerousException.type =>
          t.x // unreachable handler, no warning here
      }
    } catch {
      case MyException =>
        MyException.writeX(y) // warn
        0
    }
  }
}
