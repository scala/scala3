object Bug {

  def [A, B, Z](ab: (A, B)) pipe2(f: (A, B) => Z): Z = f(ab._1, ab._2)

  def [A, B](a: A) leftErr(b: B): A = (a, b).pipe2((a, b) => a) //Did not compile before.
  def [A, B](a: A) leftOk1(b: B): A = Tuple2(a, b).pipe2((a, b) => a) //Compiles
  def [A, B](a: A) leftOk2(b: B): A = {
    val t = (a, b)
    t.pipe2((a, b) => a) //Compiles
  }
}
