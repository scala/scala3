import language.experimental.saferExceptions
import java.io.IOException

class GenericExc[T] extends Exception

def test throws IOException, GenericExc[Int] : Unit = ()