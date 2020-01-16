package foo:
  package bar:
    object A:
      def foo = 1
  end bar
end foo
package baz:
  object B:
    def f = foo.bar.A.foo
end baz
