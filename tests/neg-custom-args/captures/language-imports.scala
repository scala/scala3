def test =
  import language.experimental.captureChecking // ok, feature enabled by -language flag
  import language.experimental.pureFunctions   // error
  1


