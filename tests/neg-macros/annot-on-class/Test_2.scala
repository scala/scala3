@voidAnnot
class A // error

class B:
  @voidAnnot
  class C // error

  def test =
    @voidAnnot
    class D // error
    ()
