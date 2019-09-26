package scala

/** A `Typeable[S, T]` (where `T <: S`) contains the logic needed to know at runtime if a value of
 *  type `S` can be downcased to `T`.
 *
 *  Unlike `ClassTag`, a `Typeable` must check the type arguments at runtime.
 */
trait Typeable[S, T <: S] extends Serializable {
  def unapply(x: S): Option[T]
}
