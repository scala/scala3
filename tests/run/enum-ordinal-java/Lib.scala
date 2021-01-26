object Lib1:
  trait MyJavaEnum[E <: java.lang.Enum[E]] extends java.lang.Enum[E]

object Lib2:
  type JavaEnumAlias[E <: java.lang.Enum[E]] = java.lang.Enum[E]
