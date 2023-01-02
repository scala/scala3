import language.experimental.saferExceptions
import java.io.*

def multipleErrors() throws IOException, SecurityException : Int = ???

@main def main : Int =
  try
    try
      multipleErrors()
    catch
      case _: IOException => ???
  catch
    case _: SecurityException => ???