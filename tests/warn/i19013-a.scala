def handle[E <: Exception](f: => Unit): Option[E] =
  try
    f
    None
  catch case e: E => Some(e) // warn

val r: RuntimeException = handle[RuntimeException](throw new Exception()).get