@main def Test = {
  case class Document()
  val expected: Document = ???
  Assertions.assert( expected == Document()) // error
}
