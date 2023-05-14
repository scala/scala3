class C
type Cap = C^

def test(c: Cap) =
  def x = () => () => c; ()
  def y = () => x()
  def z = () => x()()

