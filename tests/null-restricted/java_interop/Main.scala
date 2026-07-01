class Main:

    class Fred(val s: String, val t: Pair[String, Foo]):
        def this(s: String, t: String) =
            this(s, new Pair[String, Foo](t, new Foo))

    val fr = new Fred("green", "eggs")

    def main(args: Array[String]): Unit = {
        val s: String = "Hello world!"
        val f: Foo = new Foo
        var duppedS = f.dup(s)
        // val duppedNull = f.dup(null) // error
        f.setContent(s)
        // f.setContent(null) // error
        val p: Pair[String, Foo] = new Pair[String, Foo](s, f)
        val k = p.getKey()
        val fred = new Fred(s, p)
    }