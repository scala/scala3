package tests
package groups

/** Groups
 * @groupname foo Foo-group
 * @groupprio bazz 10
 * @groupdesc bar Description of group
 * bar
 */
class Groups {
  /** Method in group foo
   * @group foo
   */
  def groupFoo: String = ???

  /** Method in group bar
   * @group bar
   */
  def groupBar: String = ???

  /** Method in group bazz
   * @group bazz
   */
  def groupBazz: String = ???
}
