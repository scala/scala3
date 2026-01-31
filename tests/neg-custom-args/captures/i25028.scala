def foo(head: => Int, tail: (=> Int)*): List[Int] = // error
  head :: tail.toList

def bar(head: => Int, tail: (-> Int)*): List[Int] = // error
  head :: tail.toList
