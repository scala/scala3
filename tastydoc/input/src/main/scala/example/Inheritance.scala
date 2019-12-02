package example

import example.level2.Documentation

abstract class DocumentationInheritance[T, A <: Int, B >: String, -X, +Y] extends Documentation[T, A, B, X, Y] {}