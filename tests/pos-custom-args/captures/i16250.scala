// Test that import language.experimental.captureChecking is allowed
// when the feature is already enabled by a compiler flag (i16250)
def test =
  import language.experimental.captureChecking
  import language.experimental.separationChecking
  1
