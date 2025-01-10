import language.experimental.captureChecking

 trait Iterable[T] { self: Iterable[T]^ =>
   def map[U](f: T => U): Iterable[U]^{this, f}
 }

 object Test {
   def assertEquals[A, B](a: A, b: B): Boolean = ???

   def foo[T](level: Int, lines: Iterable[T]) =
     lines.map(x => x)

   def bar(messages: Iterable[String]) =
     foo(1, messages)

   val it: Iterable[String] = ???
   val msgs = bar(it)

   assertEquals(msgs, msgs)
 }
