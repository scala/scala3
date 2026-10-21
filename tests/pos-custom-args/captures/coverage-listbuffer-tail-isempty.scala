import scala.language.experimental.captureChecking

final class MiniListBuffer[A]:
  @caps.unsafe.untrackedCaptures private var first: List[A] = Nil

  def subtractOne(elem: A): this.type =
    if first.isEmpty then ()
    else
      var cursor = first
      while !cursor.tail.isEmpty && cursor.tail.head != elem do
        cursor = cursor.tail
      if !cursor.tail.isEmpty then ()
    this
