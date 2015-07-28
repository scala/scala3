object NonLocalReturn {
 def foo(a: List[Int]): Int = {
   a.foreach(x => return x)
   0
  }
}
