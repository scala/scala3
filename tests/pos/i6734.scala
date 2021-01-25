object Bug:

  extension [A, B, Z](ab: (A, B))
    def pipe2(f: (A, B) => Z): Z = f(ab._1, ab._2)

  extension [A, B](a: A)
    def leftErr(b: B): A = (a, b).pipe2((a, b) => a) //Did not compile before.
  extension [A, B](a: A)
    def leftOk1(b: B): A = Tuple2(a, b).pipe2((a, b) => a) //Compiles
  extension [A, B](a: A)
    def leftOk2(b: B): A =
      val t = (a, b)
      t.pipe2((a, b) => a) //Compiles
