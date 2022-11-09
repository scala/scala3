// minimized code
def test[A, B](a: A, b: B): A | B = a
val v0 = test("string", 1)
val v1 = test(1, "string")
val v2 = test(v0, v1)
val v3 = test(v1, v0)
val v4 = test(v2, v3)
val v5 = test(v3, v2)
val v6 = test(v4, v5)

// issue comments section examples
val t0 = Tuple1(1).toList
val t1 = (1, 2).toList
val t2 = (1, 2, 3).toList
val t3 = ("A", 2, 3).toList
val t4 = (1, "B", "C").toList
