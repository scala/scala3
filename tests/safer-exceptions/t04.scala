import scala.language.experimental.saferExceptions

@throws[Exception]
def test : Int = ???

def main : Int =
  //try
  test
  //catch
  //  case _: Exception => ???