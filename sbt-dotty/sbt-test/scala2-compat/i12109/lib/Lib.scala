package i12109

object JsonReaderDefaultValue extends LowPriorityDefaultValue {
  class ReaderDefaultValue extends scala.annotation.StaticAnnotation
}

trait LowPriorityDefaultValue {
  @JsonReaderDefaultValue.ReaderDefaultValue
  class NoDefaultValue
}
