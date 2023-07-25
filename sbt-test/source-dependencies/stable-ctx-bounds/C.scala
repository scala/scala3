package database

object C {
  def foo: Unit = {
    val rs: B.ResultSet = ???
    rs.getV[String]
  }
}
