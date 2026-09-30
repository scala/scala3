// there are spaces in the first param x def, they're important!
trait Parent:
  /**
   * @param x   
   * @param y description inherited from Parent
   */
  def method(x: Int, y: Int): Int

class Child extends Parent:
  /**
   * @param x @inheritdoc
   * @param y child description
   */
  override def method(x: Int, y: Int): Int = x + y
