case class AppUser(
    id: Long,
    firstName: Option[String],
    @SqlName("last_name") lastName: String
)

def hello: Unit =
  println(sqlFieldNamesFor[AppUser]) // Vector((lastName, last_name))
