  def test = {
    val u = 3L
    checked2(List(1L, 2L).map { k =>
       u * 2L
     })
  }
