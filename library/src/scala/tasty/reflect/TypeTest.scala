package scala.tasty
package reflect

/** A type test that can check at runtime if an object of type T is also of type U.
 *
 *  Place holder until we implement a ClassTag like abstraction that is sound for all type tests.
 *  See https://github.com/lampepfl/dotty/pull/7555
 */
type TypeTest[T, U] = scala.reflect.ClassTag[U]
