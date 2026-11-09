final class InlinedContext(var seen: Any):
  inline def handle(details: Any): Unit = seen = details
