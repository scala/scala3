object App {
  def a: String = lib.Api.Msg
  def b: Int = lib.Api.old() // forces the annotation argument of `@deprecated`
  def c: Int = lib.Api.old()
  def d: String = lib.Api.Msg
}
