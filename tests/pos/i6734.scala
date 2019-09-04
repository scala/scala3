object Bug {

  def (ab: (A, B)) pipe2[A, B, Z](f: (A, B) => Z): Z = f(ab._1, ab._2)

  def (a: A) leftErr[A, B](b: B): A = (a, b).pipe2((a, b) => a) //Did not compile before.
  def (a: A) leftOk1[A, B](b: B): A = Tuple2(a, b).pipe2((a, b) => a) //Compiles
  def (a: A) leftOk2[A, B](b: B): A = {
    val t = (a, b)
    t.pipe2((a, b) => a) //Compiles
  }
}
