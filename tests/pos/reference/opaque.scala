object Logarithms {

  opaque type Logarithm = Double

  object Logarithm {

    // These are the ways to lift to the logarithm type
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None
  }

  // Extension methods define opaque types' public APIs
  implied LogarithmOps {
    def (x: Logarithm) toDouble: Double = math.exp(x)
    def (x: Logarithm) + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def (x: Logarithm) * (y: Logarithm): Logarithm = Logarithm(x + y)
  }
}

object LogTest {
  import Logarithms._
  import Predef.{any2stringadd => _, _}

  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2
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

  val x = Item(ReadOnly)  // OK, since Permission <: Permissions

  assert( x.rights.is(ReadWrite) == false )
  assert( x.rights.isOneOf(ReadOrWrite) == true )

  // Would be a type error:
  //   assert( x.rights.isOneOf(ReadWrite) == true )

}

object o {
  opaque type T = Int
  val x: Int = id(1)
  val y: Int = identity(1)
}
def id(x: o.T): o.T = x
