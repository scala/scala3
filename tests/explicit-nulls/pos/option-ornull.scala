import java.util.Optional

class CheckResult(result: Option[Unit]):
  def foo =
    val x: Optional[Unit] = Optional.ofNullable(result.orNull)
    42
