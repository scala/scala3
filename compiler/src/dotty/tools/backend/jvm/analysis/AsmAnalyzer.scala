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

import scala.tools.asm.tree.analysis.*
import scala.tools.asm.tree.{AbstractInsnNode, MethodNode}
import dotty.tools.backend.jvm.BCodeUtils.AnalyzerExtensions


/**
 * A wrapper to make ASM's Analyzer a bit easier to use.
 */
abstract class AsmAnalyzer[V <: Value](methodNode: MethodNode, classInternalName: String, val analyzer: Analyzer[V]) {
  try {
    analyzer.analyze(classInternalName, methodNode)
  } catch {
    case ae: AnalyzerException =>
      throw new AnalyzerException(null, "While processing " + classInternalName + "." + methodNode.name, ae)
  }
  def frameAt(instruction: AbstractInsnNode): Frame[V] = analyzer.frameAt(instruction, methodNode)
}

class BasicAnalyzer(methodNode: MethodNode, classInternalName: String) extends AsmAnalyzer[BasicValue](methodNode, classInternalName, new Analyzer(new BasicInterpreter))

