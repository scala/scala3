trait A
trait B

class Overload {
  def m(ab: A&B) = ab
  def m(ab: B&A) = ab // error: double definition
}
