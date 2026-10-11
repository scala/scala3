/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package backend.jvm
package analysis

import org.objectweb.asm.tree.analysis.*
import org.objectweb.asm.tree.{AbstractInsnNode, MethodNode}
import dotty.tools.backend.jvm.BCodeUtils.AnalyzerExtensions


/**
 * A wrapper to make ASM's Analyzer a bit easier to use.
 */
abstract class AsmAnalyzer[V <: Value](methodNode: MethodNode, classInternalName: String, analyzer: Analyzer[V]) {
  // Analysis is expensive. We sometimes don't actually need to perform it.
  // (This is much easier to than remembering to always pass analyzers by name)
  private var loaded: Boolean = false

  def frameAt(instruction: AbstractInsnNode): Frame[V] = {
    if !loaded then
      try analyzer.analyze(classInternalName, methodNode)
      catch case ae: AnalyzerException => throw new AnalyzerException(null, "While processing " + classInternalName + "." + methodNode.name, ae)
      loaded = true
    analyzer.frameAt(instruction, methodNode)
  }
}

class BasicAnalyzer(methodNode: MethodNode, classInternalName: String) extends AsmAnalyzer[BasicValue](methodNode, classInternalName, new Analyzer(new BasicInterpreter))

