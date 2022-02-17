class A[T]
object o {
  // Testing compiler crash, this test should be modified when named type argument are completely implemented
  val x: A[T=Int, T=Int] = ??? // error: ']' expected, but '=' found // error: ']' expected, but '=' found
}
