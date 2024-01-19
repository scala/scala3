import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.lang.Iterable

trait HttpHeaders extends Iterable[Entry[String, String]] {
  def forEach(action: BiConsumer[String, String]): Unit = ???
}

@main def Test =
  val headers: HttpHeaders = ???
  headers.forEach((a, b) => ???)
