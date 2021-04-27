package scala.reflect

/** A shorhand for `TypeTest[Any, T]`. A `Typeable[T] contains the logic needed to
 *  know at runtime if a value can be downcasted to `T`.
 *
 *  If a pattern match is performed on a term of type `s: Any` that is uncheckable with `s.isInstanceOf[T]` and
 *  the pattern are of the form:
 *    - `t: T`
 *    - `t @ X()` where the `X.unapply` has takes an argument of type `T`
 *  then a given instance of `Typeable[T]` (`TypeTest[Any, T]`) is summoned and used to perform the test.
 */
type Typeable[T] = TypeTest[Any, T]
