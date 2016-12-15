package dotty.runtime

class DeadCodeEliminated extends RuntimeException {

  override def getMessage: String = {
    val stackTrace = getStackTrace
    val reflexiveCallIndex = stackTrace.indexWhere { elem =>
      elem.getClassName == "java.lang.reflect.Method" && elem.getMethodName == "invoke"
    }
    if (reflexiveCallIndex == -1) {
      ""
    } else {
      stackTrace.head.toString + " was invoked trough java reflection from " + stackTrace(reflexiveCallIndex + 1).toString +
        ". When dead code elimination is enabled, all methods directly called with reflection need to be annotated with @scala.EntryPoint"
    }
  }
}
