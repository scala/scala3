package test.saferExceptions.pos

import language.experimental.saferExceptions
import test.saferExceptions.pos.JavaTestFile

def checkJavaFunction : Unit = JavaTestFile.test()
/*
-- Warning: tests\safer-exceptions\pos\t02.scala:6:48 --------------------------
6 |def checkJavaFunction : Unit = JavaTestFile.test()
  |                               ^^^^^^^^^^^^^^^^^^^
  | A Java function was called (test) in a context where safer exceptions is enabled.
  | This function might throw an exception.
  | Handling of Java method is yet to be implemented.

*/
