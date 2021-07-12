val a: (1, 2, 3) = (1, 2, 3)
val b: (4, 5, 6) = (4, 5, 6)
val c: (7, 8) = (7, 8)
val d: Unit = ()
val e: (1, "foo", 10.1) = (1, "foo", 10.1)

// Zip
val r1: ((1, 4), (2, 5), (6, 6)) = a.zip(b)  // error
val r2: ((1, 7), (1, 8)) = a.zip(c)  // error
val r3: ((2, 1), (8, 2)) = c.zip(a)  // error

// Map
case class Foo[X](x: X)

val r6: (Int, Int, String) = a.map[[t] =>> Int]([t] => (x: t) => x match {  // error
  case x: Int => x * x
  case _ => ???
})

val r7: ((1, Foo[1]), (2), (3, Foo[3])) =
  a.map[[t] =>> (t, Foo[t])]( [t] => (x: t) => (x, Foo(x)) )  // error

// More Zip
val t1: Int *: Long *: Tuple = (1, 2l, 100, 200)
val t2: Int *: Char *: Tuple = (1, 'c', 33, 42)
val t3: (Int, Int) *: (Long, Long) *: Tuple = t1.zip(t2)  // error
