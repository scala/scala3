// Traits loaded from TASTy keep `NoInits` if they have lazy vals or objects, also private ones
trait WithLazy[A]:
  lazy val v = 1
  def apply(x: A): A

trait WithObject[A]:
  object O
  def apply(x: A): A

trait PrivateLazyParent:
  private lazy val v = 1
  def get = v
trait ViaPrivateLazyParent[A] extends PrivateLazyParent:
  def apply(x: A): A

trait PrivateObjectParent:
  private object O
  def get: AnyRef = O
trait ViaPrivateObjectParent[A] extends PrivateObjectParent:
  def apply(x: A): A
