trait Trait[X] {
    def f[C[_]](arg: C[X]): Int
    val a = f[(Int, String)] // error
}