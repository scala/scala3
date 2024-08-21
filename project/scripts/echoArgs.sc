// This is a Scala CLI script

val formatted =
  (for (arg, i) <- args.zipWithIndex yield
    s"[$i:$arg]").mkString(",")
println(formatted)
