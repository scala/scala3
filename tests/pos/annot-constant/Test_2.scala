//> using options -Werror -deprecation -feature

package pkg

object U {
  println(Constants_1.foo()) // The same constant in the constant pool is first unpickled here as a boolean
  println(Constants_1.BYTE) // ... and here as a byte
}
