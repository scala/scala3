type Pos = {v: Int with v >= 0}
type Matrix[T] = List[List[T]]

def length[T](v: List[T]): Pos =
  v.length.runtimeChecked

def width[T](m: Matrix[T]): Pos =
  if m.isEmpty then 0 else length(m.head)

def height[T](m: Matrix[T]): Pos =
  length(m)

def tabulate[T](rows: Pos, cols: Pos, f: (r: Pos with r <= rows, c: Pos with c <= cols) => T)
    : {r: Matrix[T] with width(r) == cols && height(r) == rows} =
  List.tabulate(rows, cols)((r, c) => f(r.runtimeChecked, c.runtimeChecked)).runtimeChecked

def transpose[T](m: Matrix[T]): {r: Matrix[T] with width(r) == height(m) && height(r) == width(m)} =
  val newWidth = height(m)
  val newHeight = width(m)
  tabulate(newHeight, newWidth, (r, c) => m(r)(c))

