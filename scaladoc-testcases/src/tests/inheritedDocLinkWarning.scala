package tests
package inheritedDocLinkWarning

package lib:
  trait Parent:
    /** Doc comment referring to [[Parent]] */
    def values: Any

class Child extends lib.Parent:
  def values = ???
