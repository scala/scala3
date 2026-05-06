import scala.language.experimental.safe

def main = Option(0) match { case Some(x) => (); case None => () }

def f = throw new Exception("test")