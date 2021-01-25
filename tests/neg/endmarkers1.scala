
def f7[T](x: Option[T]) = x match
  case Some(y) =>
  case None =>
end if  // error: misaligned end marker

object Test4:
  def f[T](x: Option[T]) = x match
    case Some(y) =>
    case None =>
  end if  // error: misaligned end marker
end Test // error: misaligned end marker

def f[T](x: Option[T]) = x match
  case Some(y) =>
  case None => "hello"
 end f  // error: The start of this line does not match any of the previous indentation widths.

