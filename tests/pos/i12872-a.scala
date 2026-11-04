class C(val d: D, val ref: Option[d.Type])
class D extends C(null, None) {
  type Type
}
