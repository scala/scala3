case class CustomException(x: Any) extends Exception("")

def handle[E](f: => Unit): Option[E] =
  try
    f
    None
  catch case CustomException(e: E) => Some(e) // warn

val r: RuntimeException = handle[RuntimeException](throw new Exception()).get
