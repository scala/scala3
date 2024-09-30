class C:
  final var v = 42 // ok

  def f =
    final val v1 = 42 // error: final modifier is not allowed on local definitions
    final lazy val v2 = 42 // error:  final modifier is not allowed on local definitions
    final def v4 = 42 // error: final modifier is not allowed on local definitions
    final var v5 = 42 // error: final modifier is not allowed on local definitions
    final type Foo = String // error: final modifier is not allowed on local definitions

    // We get a different error message here because `private` is also not a
    // local modifier token. In the future, we could always parse all tokens and
    // then flag those that are not legal at this point.
    final private val v3 = 42 // error: expected start of definition

    {
      // No error in this case, because the `given` is translated to a class
      // definition, for which `final` is redundant but not illegal.
      final given Object() // warning: modifier `final` is redundant for this definition
    }

    {
      // Also no error in this case, because we can't easily distinguish it from
      // the previous case.
      final given Object = new Object {}
    }
