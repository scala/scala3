// Chained transparent inline macro operations should compile in
// reasonable time. Before the fix, PostTyper re-transformed the
// exponentially growing call trees of Inlined nodes, causing each
// additional operation to roughly double the compile time.
// 15 additions took ~90s; this test would have timed out.
object Ops:
  extension (inline lhs: Wrapper)
    transparent inline def +(inline rhs: Wrapper): Wrapper =
      MacroHelper.add[Wrapper, Wrapper, Wrapper](lhs, rhs)

def test(): Unit =
  import Ops.*
  val a = Wrapper(1)
  val result = (
    a + a + a + a + a + a + a + a + a + a
    + a + a + a + a + a + a + a + a + a + a
    + a
  )
  println(result.value)
