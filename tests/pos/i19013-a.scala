//> using options -Werror

def handle[E <: Exception](f: => Unit): Option[E] =
  try
    f
    None
  catch case e: E @unchecked => Some(e)

val r: RuntimeException = handle[RuntimeException](throw new Exception()).get
