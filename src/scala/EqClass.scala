package scala

/** A class providing a specialized notion of equality, allowing
 *  only values in the same equality class to be compared.
 *  To be used in one of two ways:
 *
 *  1st way: When defining a class or trait `C`, simply write:
 *
 *     class C extends ... EqClass[T]
 *
 *  This makes `C` the root of a separate equality class. Any subtype
 *  of `C` can then be compared with any other subtype of `C`, but it cannot
 *  be compared with types that are not subtypes of `C`.
 *
 *  2nd way: Define `C` like this
 *
 *     class C extends ... EqClass[_]
 *
 *   or, equivalently:
 *
 *     class C extends ... EqClass[Nothing]
 *
 *  This creates a new equality class for `C`, but makes subtypes of `C` not automatically
 *  comparable with other subtypes of `C`. Instead, the rules of what is comparable to what can
 *  be encoded by giving `Eq` implicits in the companion object of `C`. Here is an example
 *  that makes `Optional` values only be comparable if their element types are comparable:
 *
 *      trait Optional[+T] extends EqClass[_]
 *      case class Some[+T](x: T) extends Optional[T]
 *      case object None extends Optional[T]
 *
 *      object Optional {
 *        def optEq[T](implicit ee: Eq[T]): Eq[Optional[T]] = Eq
 *      }
 */
trait EqClass[-T] extends Any

