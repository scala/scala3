//> using options -Xread-docs

@main def Test(): Unit = {
    val res = getDocString[scala.quoted.Quotes]
    println(res)
    assert(res.nonEmpty)
}
