package `with spaces` { // error
  class Foo
}

package +.* { // error // error
  class Bar
}

package object `mixed_*` { // error
  class Baz
}
