trait Noop {
  inline def noop: String
}

inline def boom: String = (??? : Noop).noop
def test: Unit = boom // error
