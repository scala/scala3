object Minimized:
  type Pointer[S <: Int] <: S

  type Break = Int {
    def boom: Unit
  }

  def test = {
    val ptrBreak: Pointer[Break] = ???
    ptrBreak.boom // error Required: Selectable, was boom crashes the compiler
  }