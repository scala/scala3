//> using options -Xfatal-warnings

package `with spaces` { // warn
  class Foo
}

package +.* { // warn // warn
  class Bar
}

package object `mixed_*` { // warn
  class Baz
}
// nopos-error: No warnings can be incurred under -Werror.