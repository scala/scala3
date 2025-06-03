//> using options -explain

// Tests that the error message indicates that the required type `Int` comes
// from the automatically inserted `apply` method of `String`. This note is
// inserted by `insertedApplyNote` in `Applications`.

class Config()
def renderWebsite(path: String)(using config: Config): String = ???
def renderWidget(using Config): Unit = renderWebsite("/tmp")(Config()) // error: found Config, required Int
