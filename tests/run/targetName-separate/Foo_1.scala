object Outer:
  @annotation.targetName("Bar") class Foo:
    def it: Int = 42

export Outer.Foo
