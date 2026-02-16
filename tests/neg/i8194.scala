object Test {
  private class A() { def test = 42 }
  inline def foo: Int = A().test
}
@main def main = Test.foo // error
