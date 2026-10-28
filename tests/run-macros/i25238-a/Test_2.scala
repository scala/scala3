@main def Test: Unit =
  assert(ModifierParser.use(_.rename) == "rename")
  assert(ModifierParser.use(_.rename.regional) == "rename.regional")
  assert(ModifierParser.use(_.renameNotRegional) == "renameNotRegional")
