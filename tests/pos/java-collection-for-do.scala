import java.util.{List, ArrayList}
import java.util.stream.Stream

object JavaCollectionForDo {

  for
    x <- scala.List(1, 2, 3)
    y <- List.of(1, 2, 3)
    z <- Stream.of(1, 2, 3)
  do
    println(x + y + z)

}