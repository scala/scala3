// Regression test for a crash in tpd.wrapArray when the SeqLiteral
// element type is a union of singleton primitive types (e.g. `1 | 2 | 3`).
def test: List[1 | 2 | 3] = List(1, 2, 3)
