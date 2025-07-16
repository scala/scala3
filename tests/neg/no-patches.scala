//> using options -Yno-stdlib-patches

val _ = scala.language.`3.4` // error: we do not patch `scala.language`
val _ = scala.language.experimental.captureChecking // error: we do not patch `scala.language.experimental`
val _ = Predef.summon[DummyImplicit] // error: we do not patch `scala.Predef`
