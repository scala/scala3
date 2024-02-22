//> using options -Yno-experimental

import annotation.experimental

package foo {
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions

  package bar {
    def foo = 1
  }
}

package foo2 {
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions

  package bar {
    @experimental def foo = 1
  }
}
