//> using options -language:experimental.specializedTraits

inline trait MyList[+T: Specialized]:
  def size: Int

inline trait Cons[T: Specialized](val head: T, val tail: MyList[T]) extends MyList[T]:
  def size: Int = 1 + tail.size

case object Nil extends MyList[Nothing]:
  def size: Int = 0

def sizeOf(xs: MyList[Int]): Int = xs.size

@main def Test =
  assert(sizeOf(Nil) == 0)
  assert(sizeOf(new Cons[Int](1, new Cons[Int](2, Nil) {}) {}) == 2)
