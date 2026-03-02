//> using options -Yexplicit-nulls -Yno-flexible-types

import scala.language.unsafeNulls

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._

class Test1:
  def m1 =
    val fut: CompletableFuture[Either[String, List[String]]] = ???
    fut.thenApply:
      case Right(edits: List[String]) => edits.asJava
      case Left(error: String) => throw new Exception(error)
