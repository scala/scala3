//> using options -Werror -Wunused:imports

object test1:
  object foo:
    type X = Int

object test2:
  import test1.foo

  export foo.X
