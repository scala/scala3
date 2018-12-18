
object Test {

  @inline def locally[T](x: T): T  = x

  locally {
    import Test._
    val bool: Boolean = true
  }

}
