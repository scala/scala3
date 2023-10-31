object App {
  type Id[A] >: A <: A

  val a: Array[? >: Id[? <: Int]] =
    (Array.ofDim[String](1) : Array[? >: Id[Nothing]]) // error
}