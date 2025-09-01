// scalajs: --skip

object Foo:
    def foo[X, Y, Z](x: X, y: Y)[A](z: Z, a: A): (X, Y, Z, A) = (x, y, z, a)
    def bar[X](x: X)[Y <: x.type](y: Y): (X, Y) = (x, y)

@main def Test =
    val mtds = Foo.getClass().getDeclaredMethods().filterNot(_.getName() == "writeReplace").sortBy(_.getName())
    for mtd <- mtds do
        println(s"======'${mtd.getName()}'======")
        println(mtd.getTypeParameters().mkString("<", ";", ">"))
        println(mtd.getGenericParameterTypes().mkString("(", ",", ")"))
        println(mtd.getGenericReturnType())
        println("============")