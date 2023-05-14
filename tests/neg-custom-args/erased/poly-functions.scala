object Test:
  // Poly functions with erased parameters are disallowed as an implementation restriction

  type T1 = [X] => (erased x: X, y: Int) => Int // error
  type T2 = [X] => (x: X, erased y: Int) => X // error

  val t1 = [X] => (erased x: X, y: Int) => y // error
  val t2 = [X] => (x: X, erased y: Int) => x // error

  // Erased classes should be detected too
  erased class A

  type T3 = [X] => (x: A, y: X) => X // error

  val t3 = [X] => (x: A, y: X) => y // error

