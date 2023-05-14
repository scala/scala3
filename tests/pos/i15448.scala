object SelectableBreaks1 {
  object opaques {
    opaque type Pointer[S] <: S = S
    object Pointer {
      implicit class PointerSelectable[S](private val f: Pointer[S]) extends Selectable {
        def selectDynamic(name: String): Any = ???
        def applyDynamic(name: String)(): Any = ???
      }
    }
  }
  import opaques.*

  type Break = AnyRef {
    def boom(): Nothing
  }

  def makeBreak(): Pointer[Break] = ???

  makeBreak().boom()
}

