final class PosZInt private (val value: Int) extends AnyVal

object PosZInt {
  def from(value: Int): Option[PosZInt] =
    if (value >= 0) Some(new PosZInt(value)) else None
}
