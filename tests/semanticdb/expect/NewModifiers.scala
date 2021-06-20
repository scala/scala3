object NewModifiers {
  inline val foo = "foo"
  opaque type A = Int
}

opaque type OpaqueB = Int

class NewModifiersClass {
  opaque type C = Int
  class Nested {
    opaque type NestedOpaque = Int
  }
}

trait NewModifiersTrait {
  opaque type D = Int
}
