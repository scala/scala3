import java.io.File

object Test {
  Some(new File("."))
    .map(_.listFiles).getOrElse(Array.empty) // error: undetermined ClassTag
    .map(_.listFiles) // error: missing parameter type
}
