object O:
    val a = m(2)
    def m(f: Int) =
        val a = f
        () => a

object O2:
    val func = O.a
    val a = func()