trait Noop {
  inline def noop: String
}
def test2: Unit = (??? : Noop).noop // error
