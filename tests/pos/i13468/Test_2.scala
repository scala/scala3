// https://github.com/scala/scala3/issues/13468
// First compilation round: `container` is defined (this is the state of the
// reporter's `Test.scala` while the `summon` line is still commented out).
val myOpaque: MyOpaque[8] = ???
val container = myOpaque.getContainer
