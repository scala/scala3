transparent inline def default(inline name: String): Any =
  inline if name == "Int" then 0
  else inline if name == "String" then ""
  else ???


def test =
  default("Int")
