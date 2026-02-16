//> using options -Wconf:id=E222:w

package `with spaces` { // warn
  class Foo
}

package +.* { // warn // warn
  class Bar
}

package object `mixed_*` { // warn
  class Baz
}
