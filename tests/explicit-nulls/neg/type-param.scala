/** We used to use `T >: Null <: AnyRef` to represent a reference type,
 *  and `T` cannot be `Nothing`. However, with explicit nulls, this definition
 *  is no longer valid, because `Null` is not a subtype of `AnyRef`.
 *
 *  For example:
 *  ```scala
 *  def nullOf[T >: Null <: AnyRef]: T = null
 *  ```
 *
 * We can modify the definition as following to allow only nullable type paramters.
 */

def nullOf[T >: Null <: AnyRef | Null]: T = null

def f = {
  val s1 = nullOf[String] // error: Type argument String does not conform to lower bound Null
  val s2 = nullOf[String | Null]  // ok

  val n = nullOf[Null]
  val i = nullOf[Int] // error: Type argument Int does not conform to upper bound AnyRef | Null
  val a = nullOf[Any] // error: Type argument Any does not conform to upper bound AnyRef | Null
}