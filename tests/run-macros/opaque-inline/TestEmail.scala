class TestEmail {
  def getDomain(e: EmailAddress): String = e.domainPart

  def run: Unit =
    val em = EmailAddress("a@b.c")
    assert(getDomain(em) == "b.c")
}
