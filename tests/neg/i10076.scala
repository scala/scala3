
def test: Unit = {
  def extension(a : Int) = 5
  extension (1) // error
} // error
