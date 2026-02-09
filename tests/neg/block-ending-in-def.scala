def make(): List[Int] =
  def loop(i: Int, xs: List[Int]): List[Int] =
    if i == 0 then xs else loop(i - 1, 1 :: xs) // error

