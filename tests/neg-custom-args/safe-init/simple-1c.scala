class Foo {
  val list = List(4, 6)
  val n    = len + 5       // error
  val len  = list.size     // standard typing: list is `unclassified`, this line invalid
                           // flow-sensitive typing + commit-only fields: allowed
}
