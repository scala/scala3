package scala.compiletime.testing

inline def typeChecks(inline code: String): Boolean =
  scala.compiletime.error("Cannot expand typeChecks while bootstrapping the compiler")
