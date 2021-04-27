lazy enum LazyList[+A] {  // error: sealed abstract types cannot be lazy enum
  case ::[A] (head: A, tail: LazyList[A]) extends LazyList[A]
  case Nil
}
