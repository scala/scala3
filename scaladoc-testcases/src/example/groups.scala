package example

/** Groups
 * @groupname foo Foo-group
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