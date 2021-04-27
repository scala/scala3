class Err

type Lifted[A] = Err | A

def point[O](o: O): Lifted[O] = o
extension [O, U](o: Lifted[O]) def map(f: O => U): Lifted[U] = ???

val error: Err = Err()

def ok: Int | Err =
  point("a").map(_ => if true then 1 else error)

def fail: Lifted[Int] =
  point("a").map(_ => if true then 1 else error) // error