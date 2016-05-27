object Test extends App {

  trait SpecialException extends Throwable {}

  try {
    throw new Exception
  } catch {
    case e : SpecialException => {
      println("matched SpecialException: " + e)
      assume(e.isInstanceOf[SpecialException])
    }
    case e : Exception => {
      assume(e.isInstanceOf[Exception])
    }
  }
}
