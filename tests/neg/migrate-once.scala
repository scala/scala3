//> using options -source:3.5-migration

object Test:
  for Some(x) <- Seq(Option(1)) yield x // error
  // was warn before changes, but should warn only until 3.4-migration
