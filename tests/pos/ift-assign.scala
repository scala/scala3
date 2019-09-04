class Context

object Test {
  var f: given Context => String = given _ => ""

  f = f

}