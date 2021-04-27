class WeirdNumber(v: Double) extends java.lang.Number {
  override def doubleValue = v
  override def intValue    = v.intValue
  override def longValue   = v.longValue
  override def floatValue  = v.floatValue
}
