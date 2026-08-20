import language.experimental.captureChecking

class MapConserveCapture:
  def transform(x: String)(owner: Int): String = x + owner

  def ok(xs: List[String])(owner: Int): List[String] =
    xs.mapConserve(x => transform(x)(owner))
