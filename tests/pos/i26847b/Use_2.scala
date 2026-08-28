// Inlining `g` and `use` retypes their bodies, in which the applications of
// the leading implicit parameter sections appear as regular applications
// since ApplyKind attachments are not pickled. The retyper must not
// auto-tuple the implicit arguments against the following explicit
// parameter section.
object Use:
  def test1: Int = Lib.g(1)
  def test2: Int = Debug.use(1)
