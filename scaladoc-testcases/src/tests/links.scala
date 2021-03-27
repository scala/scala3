package tests.links

object AnObject:
  def method(a: Int): Int
    = 123 + a
  val field =
    123

/**
 * Broken link, that should result a warning not break compilation
 * [[tests.links.AnObject]]
 */
class LinksTest:
  def verifyIfLinksTestIsGenerated(b: Int): Int
    = 123
