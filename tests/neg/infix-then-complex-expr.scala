@main def test = println:
  true
  && // error
  try java.lang.Boolean.valueOf("true")
