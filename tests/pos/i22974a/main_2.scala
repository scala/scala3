object Test {
  def main(): Unit =
    import pack.Maybe
    val res: Maybe[Maybe[Int]] = ???
    passThorugh(res.flatten.isDefined)
}
