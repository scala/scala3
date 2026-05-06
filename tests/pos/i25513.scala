type Foo[A, B] = (
  (A, B) match
    case (Int, Int) => (A, B),
  A
)
