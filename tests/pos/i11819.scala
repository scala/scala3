package opaquetypes

object `package`: // must be a package object

  opaque type Foo = Double // `Foo` must be an opaque type, reference must be primitive

  object Bar: // must have a wrapper object

    class Baz(val i: Foo): // must be an unqualified reference to `Foo`
      def foo(that: Any): Boolean = that match
        case that1 @ (_: Baz) => Baz.this.i == that1.i // error: symbol for `==` changed
        case _ => true
