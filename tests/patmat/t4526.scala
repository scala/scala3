// scalac: -Ycheck-all-patmat
object Test{
  def foo(a: Int) = a match {
    case 5  => "Five!"
    case 42 => "The answer."
  }

  def bar(a: (Int, Int)) = a match {
    case (5, 5)  => "Two fives!"
    case (42, 21) => "The answer and a half."
  }

  def baz(a: (Boolean, Boolean)) = a match {
    case (true, false)  => "tf"
    case (false, true) =>  "ft"
  }
}
