// scalac: -Werror -deprecation

@deprecated("42 is Jackie Robinson's retired number", since="0.1")
inline def g = 42

@main def test() = println(g)     // error

@deprecated("Usage from deprecated class is OK", since="0.1")
class C:
  def test() = println(g)     // noerror
