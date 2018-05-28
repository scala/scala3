import java.io.BufferedReader

class Test {
  def loop(reader: BufferedReader): Unit = reader.read match {
    case 'a' | 'A' =>
    case 's' | 'S' =>
    case 'r' | 'R' =>
    case 'r' =>
    case _ =>
  }
}