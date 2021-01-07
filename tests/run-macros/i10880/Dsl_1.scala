trait Ent(name: String)
case class MyContent(key: String, value: String)
case class MyInsert(key: String)

object Dsl {
  inline def ent: Ent = new Ent("something") {}
  extension (ent: Ent)
    inline def content(inline ins: MyInsert) = MyContent(ins.key, "blah")
}
