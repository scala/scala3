//> using options -experimental

import scala.annotation.unroll

case class UnrolledCase(
    s: String,
    y: Boolean = true,
) {
  def foo: String = s + y

  final def copy(s: String = this.s, @unroll y: Boolean = this.y): UnrolledCase = // error
    new UnrolledCase(s, y)
}

object UnrolledCase {
  def apply(
    s: String,
    @unroll y: Boolean = true // error
  ): UnrolledCase =
    new UnrolledCase(s, y)

  def fromProduct(@unroll p: Product = EmptyTuple): UnrolledCase = { // error
    val s = p.productElement(0).asInstanceOf[String]
    val y = p.productElement(1).asInstanceOf[Boolean]
    UnrolledCase(s, y)
  }
}
