trait A
trait B

class Base{
  def m(ab: A&B) = ab
}
class Derived extends Base{
  override def m(ab: B&A) = ab // unexpected error
}
