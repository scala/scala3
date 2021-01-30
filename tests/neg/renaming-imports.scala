def test =
  object foo:
    var x = 0
  import foo as f  // error
  f.x

