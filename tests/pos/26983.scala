object O:
  object I:
    object CCC
  export I.*
  val good: I.CCC.type = CCC
  val bad: CCC.type = CCC
