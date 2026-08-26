package inheritdoc

trait Parent:
  /** Parent documentation.
   *
   * @param x parent parameter
   * @return parent result
   */
  def method(x: Int): Int

class Child extends Parent:
  /** @inheritdoc
   *
   * @param x child parameter
   * @return child result
   */
  override def method(x: Int): Int = x