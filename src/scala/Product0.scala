package scala

object Product0 {
  def unapply(x: Product0): Option[Product0] =
    Some(x)
}

/** Product1 is a cartesian product of 1 component.
  *  @since 2.3
  */
trait Product0 extends Any with Product {
  /** The arity of this product.
    *  @return 1
    */
  override def productArity = 0


  /** Returns the n-th projection of this product if 0 < n <= productArity,
    *  otherwise throws an `IndexOutOfBoundsException`.
    *
    *  @param n number of the projection to be returned
    *  @return  same as `._(n+1)`, for example `productElement(0)` is the same as `._1`.
    *  @throws  IndexOutOfBoundsException
    */

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int) = throw new IndexOutOfBoundsException(n.toString())

}