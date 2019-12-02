
def f7[T](x: Option[T]) = x match
  case Some(y) =>
  case None =>
end if  // error: misaligned end marker

object Test4 with
  def f[T](x: Option[T]) = x match
    case Some(y) =>
    case None =>
  end if  // error: misaligned end marker
end Test // error: misaligned end marker

def f[T](x: Option[T]) = x match
  case Some(y) =>
  case None => "hello"
  end f  // error: misaligned end marker

