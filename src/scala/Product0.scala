/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala

/** A class for Product0 which was missing from the scala distribution. */
object Product0 {
  def unapply(x: Product0): Option[Product0] =
    Some(x)
}

trait Product0 extends Any with Product {

  override def productArity = 0

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int) =
    throw new IndexOutOfBoundsException(n.toString())
}
