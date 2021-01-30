import java.{lang as jl}

import language.`3.0-migration`

// This file is different from `tests/neg/extend-java-enum.scala` as we
// are testing that it is illegal to *not* pass arguments to jl.Enum
// in 3.0-migration

final class C extends jl.Enum[C] // error

object O extends jl.Enum[O.type] // error

trait T extends jl.Enum[T] // ok
class Sub extends T // error

abstract class Color(name: String, ordinal: Int) extends java.lang.Enum[Color](name, ordinal) // ok
val foo = new java.lang.Enum[Color] {} // error
