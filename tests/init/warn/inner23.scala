
class Trees {
  class ValDef {
    def setMods(x: Int) = name.size
  }

  class EmptyValDef extends ValDef {
    setMods(5)
  }

  val theEmptyValDef = new EmptyValDef
  val name = "hello"                      // warn
}
