import language.experimental.erasedDefinitions

object Test:
  type T1 = [X] => (erased x: X, y: Int) => Int
  type T2 = [X] => (x: X, erased y: Int) => X

  val t1 = [X] => (erased x: X, y: Int) => y
  val t2 = [X] => (x: X, erased y: Int) => x

  class A extends compiletime.Erased

  type T3 = [X] => (x: A, y: X) => X

  val t3 = [X] => (x: A, y: X) => y
