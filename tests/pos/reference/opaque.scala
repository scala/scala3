object Logarithms {

  opaque type Logarithm = Double

  object Logarithm {

    // These are the ways to lift to the logarithm type
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None
  }

  // Extension methods define opaque types' public APIs
  extension (x: Logarithm)
    def toDouble: Double = math.exp(x)
    def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def * (y: Logarithm): Logarithm = x + y
}

object LogTest {
  import Logarithms.*
  import Predef.{any2stringadd as _, *}

  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2
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

  val roItem = Item(Read)  // OK, since Permission <: Permissions
  val rwItem = Item(ReadWrite)
  val noItem = Item(NoPermission)

  assert( roItem.rights.is(ReadWrite) == false )
  assert( roItem.rights.isOneOf(ReadOrWrite) == true )

  assert( rwItem.rights.is(ReadWrite) == true )
  assert( rwItem.rights.isOneOf(ReadOrWrite) == true )

  assert( noItem.rights.is(ReadWrite) == false )
  assert( noItem.rights.isOneOf(ReadOrWrite) == false )

  // Would be a type error:
  //   assert( roItem.rights.isOneOf(ReadWrite) == true )
}

object o {
  opaque type T = Int
  val x: Int = id(1)
  val y: Int = identity(1)
}
def id(x: o.T): o.T = x
