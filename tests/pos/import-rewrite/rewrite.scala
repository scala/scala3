package file
import java.io.{File as JFile, *}, StreamTokenizer.TT_EOF as eof

object Main {
  Seq("").map(File.apply)
  // def name(file: File) = file.name
}
