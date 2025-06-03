import scala.language.future

def test =
  <foo/> match // error
    case <foo/> => 1 // error
  <bar></bar> // error
