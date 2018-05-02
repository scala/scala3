object Trees {
  class ValDef {
    def setMods(x: Int) = name.size           // error
  }

  class EmptyValDef extends ValDef {
    setMods(5)                                // error
  }

  val theEmptyValDef = new EmptyValDef        // error
  val name = "hello"
}
