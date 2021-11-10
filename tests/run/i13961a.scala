import language.experimental.saferExceptions

trait Decoder[+T]:
  def apply(): T

given Decoder[Int throws Exception] = new Decoder[Int throws Exception]:
  def apply(): Int throws Exception = 1

@main def Test(): Unit =
  import unsafeExceptions.canThrowAny
  summon[Decoder[Int throws Exception]]()