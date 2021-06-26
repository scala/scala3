package scala

/** Parent trait that indicates capturing. Example usage:
 *
 *   class Foo(using ctx: Context) extends Holds[ctx | CanThrow[Exception]]
 */
trait Retains[T]
