extension (self: Int) {
  import self.*             // error
  opaque type Maybe[+A] = A // error
  type B = self.type        // error
  val x: Int = self         // error
  var z: Int = self         // error
}

extension (self: Any)
  import self.*             // error
  opaque type X[+A] = A     // error
  type Y = self.type        // error
  val x: Int = self         // error
  var z: Int = self         // error
