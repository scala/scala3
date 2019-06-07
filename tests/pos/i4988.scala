object Trees {
  class PackageDef
  class ValDef
}

class ReplCompiler {
  type PackageDef = Trees.PackageDef
  type ValDef = Trees.ValDef

  def errors[A]: A = ???
  def success: ValDef = ???

  def typeCheck(cond: Boolean): ValDef =
    if (cond) success else errors // failure goes away with `errors[ValDef]`
}
