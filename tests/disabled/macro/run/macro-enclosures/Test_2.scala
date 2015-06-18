object Test extends dotty.runtime.LegacyApp {
  test.Test.test
}

package test {
  object Test {
    def test = {
      Macros.foo
    }
  }
}