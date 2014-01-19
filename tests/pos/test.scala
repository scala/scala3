object test {
  object foo extends scala.reflect.io.Streamable.Chars {
    val lns = lines()
    val pkgLines = lns collect { case x if x startsWith "package " => x stripPrefix "package" trim }
  }

}