// Test that match types can extract path-dependent abstract types out of singleton types

trait Base:
  type Value

  def getValue(): Value
  def setValue(v: Value): Unit
end Base

object Extractor:
  type Helper[X] = Base { type Value = X }

  type Extract[B <: Base] = B match
    case Helper[x] => x
end Extractor

object Test:
  import Extractor.Extract

  /* As is, this is a bit silly, since we could use `b.Value` instead. However,
   * in larger examples with more indirections, it is not always possible to
   * directly use the path-dependent version. See i21402 for a real-world use
   * case.
   */
  def foo(b: Base): Extract[b.type] = b.getValue()
  def bar(b: Base, v: Extract[b.type]): Unit = b.setValue(v)
end Test
