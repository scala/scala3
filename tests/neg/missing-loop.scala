import annotation.*
val n = 1024
val dice = Iterator.continually(42)

// block ending in def has a type mismatch
def make(): List[Int] =
  @tailrec def loop(i: Int, xs: List[Int]): List[Int] =
    if i == 0 then xs else loop(i - 1, (dice.next() % n) :: xs) // error

def f(): Unit =
  return println(loop(0, Nil))
  @tailrec def loop(i: Int, xs: List[Int]): List[Int] =
    if i == 0 then xs else loop(i - 1, (dice.next() % n) :: xs)
