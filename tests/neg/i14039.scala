val entries = Seq.newBuilder[Any]
inline def error(): Any = compiletime.error("my error")
inline def get(): Unit = entries += error()
def test = get() // error
