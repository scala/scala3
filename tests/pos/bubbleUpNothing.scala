trait T {
  def apply(a1: String, a2: String, a3: String): String = a3
}

object Test {
  def test1 =
    ??? // Nothing.String.CharSequence.String.CharSequence.String
      .toString.subSequence(0, 1).toString.subSequence(0, 1).toString

  def test2 =
    (new T {})
      .apply(???, "b", "c")
      .subSequence(0, 1).toString.subSequence(0, 1).toString

  def test3 =
    (new T {})
      .apply("a", ???, "c")
      .subSequence(0, 1).toString.subSequence(0, 1).toString

  def test4 =
    (new T {})
      .apply("a", "b", ???)
      .subSequence(0, 1).toString.subSequence(0, 1).toString

  def test5 =
    (new T {})
      .apply("a", "b", ???)
      .subSequence(0, 1).toString.subSequence(0, 1).toString

  def test6 =
    (if (???) "a" else "b")
      .subSequence(0, 1).toString.subSequence(0, 1).toString

  def test7 =
    { ???; "b"; "c" }
      .subSequence(0, 1).toString.subSequence(0, 1).toString

  def test8 =
    { "a"; ???; "c" }
      .subSequence(0, 1).toString.subSequence(0, 1).toString

  def test9 =
    { "a"; "b"; ??? }
      .toString.subSequence(0, 1).toString.subSequence(0, 1).toString

  def test10: Unit = {
    def fail = throw new IllegalArgumentException("")
  }

  def test11: Unit = {
    trait Context
    trait Type
    trait Tree {
      def withType(tpe: Type)(implicit ctx: Context): Tree = this
    }

    def readTree()(implicit ctx: Context): Any =
      (new Tree {}).withType(???)(ctx).withType(???)
  }
}
