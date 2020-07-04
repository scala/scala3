object Test:

  type Matrix = Array[Array[Double]]
  type Vector = Array[Double]

  extension (m: Matrix)
    def nRows = m.length
    def nCols = m(0).length
    def row(i: Int): Vector = m(i)
    def col(j: Int): Vector = Array.tabulate(m.length)(i => m(i)(j))

  def pairwise(m: Matrix) =
    for
      i <- 0 until m.nRows
      j <- 0 until m.nCols
    yield
      m.row(i).zip(m.row(j)).map(_ - _).sum
