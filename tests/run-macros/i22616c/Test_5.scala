@main def Test =
  println(Macro.myMacro[SealedTrait3.B[Any]]())
