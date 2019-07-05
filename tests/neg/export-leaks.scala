// Check that exports do not leak private types
object Signature {

  private type T

  object O1 {
    private[Signature] def bar: T = ???
  }
  export O1._ // error: non-private method bar refers to private type T

  object O2 {
    private[Signature] val foo: T = ???
  }
  export O2._ // OK
      // The reason this works is that private escape checking only looks at real private members,
      // not at private[C] members. So, by itself the expansion of the export
      //   <stable> def foo: O2.foo.type = O2.foo
      // is legal. The underlying type of `O2.foo.type` does violate no-escape rules, but we do not
      // check for it. Maybe we should. But then the question comes up whether we should
      // also check all possible supertypes of a type for privacy violations. These are more
      // general questions that are not related to exports.
}