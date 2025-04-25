class C
object C:
  extension (c: C) def equals(that: Any): Boolean = false // warn

object X:
  class C
  opaque type D <: C = C
  object D:
    extension (d: D) def equals(that: Any): Boolean = false // warn

object Upperbound1:
  opaque type MyString[+T] <: String = String
  extension (arr: MyString[Byte]) def length: Int = 0 // warn

object Upperbound2:
  opaque type MyString[+T] <: String = String
  extension [T <: MyString[Byte]](arr: T) def length: Int = 0 // warn

object Upperbound3:
  opaque type MyString[+T] <: String = String
  extension [T](arr: T) def length: Int = 0 // nowarn

object NonUpperbound1:
  opaque type MyString[+T] = String
  extension (arr: MyString[Byte]) def length: Int = 0 // nowarn

object NonUpperbound2:
  opaque type MyString[+T] = String
  extension [T <: MyString[Byte]](arr: T) def length2: Int = 0 // nowarn

object NonUpperbound3:
  opaque type MyString[+T] = String
  extension [T](arr: T) def length: Int = 0 // nowarn

object NonUpperbound4:
  opaque type MyString = String
  extension (arr: MyString) def length: Int = 0 // nowarn
