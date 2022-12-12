transparent trait ID
case class UserName(name: String) extends ID
case class Password(hash: Int) extends ID

val password: Password = Password(123)
val name = UserName("Eve")
val res = if ??? then name else password
val _: UserName | Password = res