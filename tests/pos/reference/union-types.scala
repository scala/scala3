package unionTypes

object t1 {

  type Hash = Int

  case class UserName(name: String)
  case class Password(hash: Hash)

  def help(id: UserName | Password) = {
    val user = id match {
      case UserName(name) => lookupName(name)
      case Password(hash) => lookupPassword(hash)
    }
  }

  def lookupName(name: String) = ???
  def lookupPassword(hash: Hash) = ???

}

object t2 {
  import t1.*

  trait Admin
  trait UserData

  trait L { def lookup(admin: Admin): Object }

  case class UserName(name: String) extends L {
    def lookup(admin: Admin): UserData = ???
  }
  case class Password(hash: Hash) extends L {
    def lookup(admin: Admin): UserData = ???
  }

}
