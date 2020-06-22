object i9202 {
  opaque type SomeUnrelatedOpaque = Int
  class A[T](val d: T)
  extension Ex2SameNameToBeToImport on [T] (x: A[T]) { def value: T = x.d }
}