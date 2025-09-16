//> using options -Yno-stdlib-patches

val _ = scala.language.`3.4` // error: we do not patch `scala.language`
val _ = scala.language.experimental.modularity // error: we do not patch `scala.language.experimental`
