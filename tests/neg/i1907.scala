import java.io.File

object Test {
  Some(new File("."))
    .map(_.listFiles.nn).getOrElse(Array.empty) // error: undetermined ClassTag
    .map(_.nn.listFiles)
}
