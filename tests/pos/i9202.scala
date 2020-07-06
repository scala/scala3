object i9202 {
  opaque type SomeUnrelatedOpaque = Int
  class A[T](val d: T)
  extension [T] (x: A[T]) { def value: T = x.d }
}