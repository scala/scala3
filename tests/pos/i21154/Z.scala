//> using options -Ytest-pickler-check

// in the original issue https://github.com/scala/scala3/issues/21154, the non-deterministic tasty
// depends on the order of compilation of files, the use-site (A.scala) has to come first,
// and the file defining the enum has to come second (Z.scala), A.scala in namer will force Z to complete.
sealed trait Z

object Z:
  class AOptions() extends Z
  class BOptions() extends Z
  class COptions() extends Z
