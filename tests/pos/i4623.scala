trait Test {
  type A <: Any { type T }
  type B <: Any { type T }
  type C <: A with B { type T }

  type D <: List[A] with List[B] { type T }
}
