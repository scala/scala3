trait Foo:
  type TC[T]
  type A[X: TC]                     // error
  type C = [X: TC] =>> List[X]      // error
  type D = [X: TC] => () => List[X] // allowed context bound
