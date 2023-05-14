import language.`future`
def Test =
  for (a, b) <- List("a","b","c").lazyZip(List(1,2,3)) do println(s"$a$b")