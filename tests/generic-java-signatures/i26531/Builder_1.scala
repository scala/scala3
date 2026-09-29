package i26531

import java.util.function.Consumer

final class Builder[T]:
  def accept[M <: T](clazz: Class[M], consumer: Consumer[M]): Unit = ()
