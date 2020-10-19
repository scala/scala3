// nopos-error

extension [T](a: T) def foo: String = a.toString

@main def test = printExpr(Some(1).foo)
