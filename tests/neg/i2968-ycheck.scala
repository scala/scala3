// scalac: -Werror -deprecation

def f = 0x01030507 << 36L == 42   // error
//def f = 0x01030507 << 36L == 271601776  //?error

@deprecated("Usage from deprecated class is OK", since="0.1")
class C:
  def test = 0x01030507 << 36L == 271601776  // noerror
