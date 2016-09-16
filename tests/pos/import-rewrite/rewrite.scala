package file
import java.io.{File => JFile, _}, StreamTokenizer.{TT_EOF => eof}

object Main {
  Seq("").map(File.apply)
  // def name(file: File) = file.name
}
