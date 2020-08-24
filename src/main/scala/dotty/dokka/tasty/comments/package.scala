package dotty.dokka.tasty.comments

import org.jetbrains.dokka.model.{doc => dkkd}

object kt:
  import kotlin.collections.builders.{ListBuilder => KtListBuilder, MapBuilder => KtMapBuilder}

  def emptyList[T] = new KtListBuilder[T]().build()
  def emptyMap[A, B] = new KtMapBuilder[A, B]().build()

object dkk:
  def text(str: String) =
    dkkd.Text(str, kt.emptyList, kt.emptyMap)

