package scala.quoted.show

/** Annotation used inside a quote to give a custom name to a definition.
 *  The `name` argument must be a literal String.
 *
 *  Usage:
 *  ```scala
 *  def let(name: String)(value: Expr[Int])(in: Expr[Int] => Expr[Int]): Expr[Int] = '{
 *    @showName(${Expr(name)})
 *    val x = $value
 *    ${ in('x) }
 *  }
 *  ```
 *  then using it in
 *  ```scala
 *  let("myVal")('{4})(x => '{ $x + 1}).show
 *  ```
 *  will retuns the code
 *  ```scala
 *  val myVal = 4
 *  myVal + 1
 *  ```
 */
class showName(name: String) extends scala.annotation.Annotation
