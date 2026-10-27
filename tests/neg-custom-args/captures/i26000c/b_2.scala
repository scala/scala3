val keep: Cell[File] = Cell(File())
def leak(): File =
  Lib.cell.elem = (x: File) => keep.elem = x // error
  Lib.run()
  keep.elem
