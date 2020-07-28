package build

// This file is an exact copy of the file of the same name in scala-js/scala-js

import org.scalajs.linker.interface._

object TestSuiteLinkerOptions {

  def semantics(s: Semantics): Semantics = {
    import Semantics.RuntimeClassNameMapper

    s.withRuntimeClassNameMapper(
        RuntimeClassNameMapper.keepAll().andThen(
            RuntimeClassNameMapper.regexReplace(
                raw"""^org\.scalajs\.testsuite\.compiler\.ReflectionTest\$$RenamedTestClass$$""".r,
                "renamed.test.Class")
        ).andThen(
            RuntimeClassNameMapper.regexReplace(
                raw"""^org\.scalajs\.testsuite\.compiler\.ReflectionTest\$$Prefix""".r,
                "renamed.test.byprefix.")
        ).andThen(
            RuntimeClassNameMapper.regexReplace(
                raw"""^org\.scalajs\.testsuite\.compiler\.ReflectionTest\$$OtherPrefix""".r,
                "renamed.test.byotherprefix.")
        )
    )
  }

  def moduleInitializers: List[ModuleInitializer] = {
    val module = "org.scalajs.testsuite.compiler.ModuleInitializers"
    List(
        ModuleInitializer.mainMethod(module, "mainNoArgs"),
        ModuleInitializer.mainMethodWithArgs(module, "mainWithArgs"),
        ModuleInitializer.mainMethodWithArgs(module, "mainWithArgs", List("foo", "bar")),
        ModuleInitializer.mainMethod(module + "$NoLinkedClass", "main"),
        ModuleInitializer.mainMethod(module + "$WithLinkedClass", "main")
    )
  }
}
