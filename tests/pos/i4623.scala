trait Test {
  type A <: Any { type T }
  type B <: Any { type T }
  type C <: A & B { type T }

  type D <: List[A] & List[B] { type T }
}
