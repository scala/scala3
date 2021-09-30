opaque type LabelTagged[TLabel <: Singleton & String, TValue] = TValue

object LabelTagged:
  def apply[TLabel <: Singleton & String, TValue]
  (
    label: TLabel,
    value: TValue,
  )
  : LabelTagged[TLabel, TValue] = value

extension[TLabel <: Singleton & String, TValue] (labelTagged: LabelTagged[TLabel, TValue])
  def value
  : TValue = labelTagged

  def label
  (using label: ValueOf[TLabel])
  : TLabel
  = label.value

@main def hello(): Unit = {
  val foo: LabelTagged["foo", Int] = LabelTagged("foo", 10)
  println(label(foo))  // OK
  println(foo.label)   // was error, now OK
}
