package tests.site


package some.other:
  class SomeOtherPackage

class SomeClass:
  def method(a: Int): Int
    = 123 + a
  val field =
    123

/**
 * Broken link, that should result a warning not break compilation
 * [[tests.links.AnObject]]

 */
class BrokenLink:
  def verifyIfLinksTestIsGenerated(b: Int): Int
    = 123

/**
 * [[tests.links.some.other.SomeOtherPackage]]
 */
class OtherPackageLink

/**
 * [[tests.links.SomeClass]]
 */
class SamePackageLink


/**
 * Broken link, that should result a warning not break compilation
 * [[tests.links.AnObject]]

 */
class BrokenLinkWiki:
  def verifyIfLinksTestIsGenerated(b: Int): Int
    = 123

/**
 * [[tests.links.some.other.SomeOtherPackage]]
 *  @syntax wiki
 */
class OtherPackageLinkWiki

/**
 * [[tests.links.SomeClass]]
 *  @syntax wiki
 */
class SamePackageLinkWiki