class Test { // error: class Test cannot be instantiated

  opaque type FlagSet = Int

  opaque type Flag <: FlagSet = String  // error: type String outside bounds  <: Test.this.FlagSet

  object Flag {
    def make(s: String): Flag = s
  }

  val f: Flag = Flag.make("hello")
  val g: FlagSet = f

}

object Access {

  opaque type Permissions = Int
  opaque type PermissionChoice = Int
  opaque type Permission <: Permissions & PermissionChoice = Int

  def (x: Permissions) & (y: Permissions): Permissions = x & y
  def (x: PermissionChoice) | (y: PermissionChoice): PermissionChoice = x | y
  def (x: Permissions) is (y: Permissions) = (x & y) == y
  def (x: Permissions) isOneOf (y: PermissionChoice) = (x & y) != 0

  val NoPermission: Permission = 0
  val ReadOnly: Permission = 1
  val WriteOnly: Permission = 2
  val ReadWrite: Permissions = ReadOnly & WriteOnly
  val ReadOrWrite: PermissionChoice = ReadOnly | WriteOnly
}

object User {
  import Access._

  case class Item(rights: Permissions)

  val p1: Permissions = ReadOrWrite  // error
  val p2: PermissionChoice = ReadWrite // error

  val x = Item(ReadOnly)

  assert( x.rights.is(ReadWrite) == false )
  assert( x.rights.isOneOf(ReadOrWrite) == true )

  assert( x.rights.isOneOf(ReadWrite) == true ) // error: found Permissions, required: PermissionChoice
}