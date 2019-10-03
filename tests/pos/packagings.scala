package foo with
  package bar with
    object A with
      def foo = 1
  end bar
end foo
package baz with
  object B with
    def f = foo.bar.A.foo
end baz
