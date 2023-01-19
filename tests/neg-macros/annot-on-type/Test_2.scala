@voidAnnot
type A // error

object B:
  @voidAnnot
  type C // error

  def test =
    @voidAnnot
    type D // error
    ()
