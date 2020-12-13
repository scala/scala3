def f(using scala.quoted.Quotes) =
    '{
        val x = ${
          ???
        }
        x
     }

type HasPath = {
  def getPath: String
}
type HashPath2 = Any {
  def getPath: String
}
val x = new {
  val y = 1
}
val y = new AnyRef {
  val y = 1
}

