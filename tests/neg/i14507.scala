class C:
  def test = g
  def f(b: Boolean): Int =
    if b then // error
    end if
    42
  def g = 27
end C