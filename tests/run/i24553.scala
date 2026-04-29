// scalajs: --skip
class Foo:
    val hello = 1337
    val x: hello.type = ???

@main def Test =
    val mtds = classOf[Foo].getMethods().sortBy(_.toGenericString())
    for mtd <- mtds do println(mtd.toGenericString())
