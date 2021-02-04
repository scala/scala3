package tests

package commonlinks:
  class SomeOtherPackage:
    def method = 123

  object SomeOtherPackage

  enum SomeOtherEnum:
    case A
    case B



package wikilinks:
  class SomeClass:
    def method = 123

  /**
   * [[AnNonExisitingObject]]
   * [[SomeClass!.ala]]
   * @syntax wiki
   */
  class BrokenLinks

  /**
   * [[tests.commonlinks.SomeOtherPackage]]
   * @syntax wiki
   */
  class OtherPackageLink

  /**
   * [[tests.commonlinks.SomeOtherPackage!method]]
   * [[tests.commonlinks.SomeOtherEnum!A]]
   * @syntax wiki
   */
  class OtherPackageMembers
  /**
   * [[SomeClass]]
   * @syntax wiki
   */
  class SamePackageLink

  /**
   * [[SomeClass.method]]
   * @syntax wiki
   */
  class SamePackageMembers

// It should be exact copy of wikilinks
package mdlinks:
  class SomeClass:
    def method = 123

  /**
   * [[AnNonExisitingObject]]
   * [[SomeClass!.ala]]
   */
  class BrokenLinks

  /**
   * [[tests.commonlinks.SomeOtherPackage]]
   */
  class OtherPackageLink

  /**
   * [[tests.commonlinks.SomeOtherPackage!method]]
   * [[tests.commonlinks.SomeOtherEnum!A]]
   * @syntax wiki
   */
  class OtherPackageMembers
  /**
   * [[SomeClass]]
   */
  class SamePackageLink

  /**
   * [[SomeClass.method]]
   */
  class SamePackageMembers