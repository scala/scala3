package dotty.tools.pc.base

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.collection.immutable
import scala.language.unsafeNulls
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.pc.CancelToken

abstract class BaseCodeActionSuite extends BasePCSuite:

  def cancelToken: CancelToken = EmptyCancelToken

  def params(
      code: String
  ): (String, String, Int) =
    val filename = "A.scala"
    val targetRegex = "<<(.+)>>".r
    val target = targetRegex.findAllMatchIn(code).toList match
      case immutable.Nil => fail("Missing <<target>>")
      case t :: immutable.Nil => t.group(1)
      case _ => fail("Multiple <<targets>> found")
    val code2 = code.replace("<<", "").replace(">>", "")
    val offset = code.indexOf("<<") + target.length()
    val file = tmp.resolve(filename)
    Files.write(file, code2.getBytes(StandardCharsets.UTF_8))

    testingWorkspaceSearch.inputs.update(filename, code)
    (code2, target, offset)
