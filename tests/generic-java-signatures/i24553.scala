class Foo:
    val hello = 1337
    val x: hello.type = ???

@main def Test =
    val mtds = classOf[Foo].getDeclaredMethods().sortBy(_.getName())
    for mtd <- mtds do println(mtd.toGenericString())
