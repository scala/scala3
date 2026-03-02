// scalajs: --skip

class Foo:
  def bar[B](x: B): x.type = x

@main def Test =
  for mtd <- classOf[Foo].getDeclaredMethods.sortBy(_.getName) do
    println(mtd.toGenericString)
