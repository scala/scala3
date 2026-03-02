@main def Test() =
  println(Macro.logPrimaryConstructor[JavaClass])
  println(Macro.logPrimaryConstructor[JavaClassEmpty])
  println(Macro.logPrimaryConstructor[JavaClassPrivate])
  println(Macro.logPrimaryConstructor[JavaClassStartsWithPrivate])
  println(Macro.logPrimaryConstructor[JavaClassParam[Int]])
