@voidAnnot
object A // error

object B:
  @voidAnnot
  object C // error

  def test =
    @voidAnnot
    object D // error
    ()