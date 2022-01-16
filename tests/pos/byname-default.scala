// Extraction of a failing case in scalaSTM
object Test:

  def apply[A](init: => A = null.asInstanceOf[A]) = init

