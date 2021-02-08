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

  extension (x: Permissions)
    def & (y: Permissions): Permissions = x | y
  extension (x: PermissionChoice)
    def | (y: PermissionChoice): PermissionChoice = x | y
  extension (granted: Permissions)
    def is(required: Permissions) = (granted & required) == required
  extension (granted: Permissions)
    def isOneOf(required: PermissionChoice) = (granted & required) != 0

  val NoPermission: Permission = 0
  val Read: Permission = 1
  val Write: Permission = 2
  val ReadWrite: Permissions = Read | Write
  val ReadOrWrite: PermissionChoice = Read | Write
}

object User {
  import Access.*

  case class Item(rights: Permissions)

  val p1: Permissions = ReadOrWrite  // error
  val p2: PermissionChoice = ReadWrite // error

  val x = Item(Read)

  assert( x.rights.is(ReadWrite) == false )
  assert( x.rights.isOneOf(ReadOrWrite) == true )

  assert( x.rights.isOneOf(ReadWrite) == true ) // error: found Permissions, required: PermissionChoice
}