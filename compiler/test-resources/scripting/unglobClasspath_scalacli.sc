// This file is a Scala CLI script.

import dotty.tools.tasty.TastyFormat
//     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//     not visible on default classpath, "compiler/test/dotty/tools/scripting/ClasspathTests.scala"
//     adds it to classpath via a compiler argument `-classpath 'org/scala-lang/tasty-core_3/$VERSION/*'`

val cp = sys.props("java.class.path")
printf("unglobbed classpath: %s\n", cp)
