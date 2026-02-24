def test(input: Option[List[XmlEvent]]) =
  val works =
    expect.same(List.empty[XmlEvent], input.get)
  val fails = input.map: tokens =>
    expect.same(List.empty[XmlEvent], tokens)
