object Lib1 with
  trait MyJavaEnum[E <: java.lang.Enum[E]] extends java.lang.Enum[E]

object Lib2 with
  type JavaEnumAlias[E <: java.lang.Enum[E]] = java.lang.Enum[E]
