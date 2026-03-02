import java.util.stream.*
import java.util.function.*

val map: java.util.Map[String, String] = Stream.of("1", "2", "3").collect(Collectors.toMap(
  (s: String) => s,
  Function.identity(),
  {
    case ("1", "1") => "1"
    case (_, l) => l
  }
))
