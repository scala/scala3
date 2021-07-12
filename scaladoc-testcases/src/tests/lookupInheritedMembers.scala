package tests
package lookupInheritedMembers.pack1 {
  class A:
    def x = 1
    val y = 1
    type MyType

}
package lookupInheritedMembers.pack2 {
  class B extends tests.lookupInheritedMembers.pack1.A
}

package lookupInheritedMembers {
  /**
   * [[tests.lookupInheritedMembers.pack2.B.x]]
   * [[tests.lookupInheritedMembers.pack2.B.y]]
   * [[tests.lookupInheritedMembers.pack2.B.MyType]]
   *
   */
  class LookupInheritedMembers

  /**
   * This look up is problematic, because is lazyloaded by dotty.
   *
   * [[java.util.Formatter]]
   */
  class JavaInheritedMembers
}

