// Under explicit nulls, a named-tuple pattern should still resolve its
// element names even when the selector type is nullable (e.g. coming from a
// Java-defined generic method such as `java.util.stream.Stream.filter`).
import java.util.stream.{Stream => JStream}

class Store
class OriginalPath

object Test:

  type SearchRootResult = (searchRoot: Store, originalPath: OriginalPath)

  def test(stream: JStream[SearchRootResult]): JStream[SearchRootResult] =
    stream.filter { case (searchRoot = sr) =>
      sr.isInstanceOf[Store]
    }
