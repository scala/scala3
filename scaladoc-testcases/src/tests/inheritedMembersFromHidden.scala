package tests
package inheritedMembersFromHidden

private[inheritedMembersFromHidden] trait HiddenTrait { //unexpected
  def method: Unit
    = ???
}

object PublicObject extends HiddenTrait //expected: object PublicObject
