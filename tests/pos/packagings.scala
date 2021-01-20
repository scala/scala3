package foo:
  package bar:
    object A with
      def foo = 1
  end bar
end foo
package baz:
  object B with
    def f = foo.bar.A.foo
end baz
