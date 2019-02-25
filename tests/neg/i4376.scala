object App {
  type Id[A] >: A <: A

  val a: Array[_ >: Id[_ <: Int]] =
    (Array.ofDim[String](1) : Array[_ >: Id[Nothing]]) // error
}