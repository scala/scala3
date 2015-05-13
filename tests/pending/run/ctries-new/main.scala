import scala.reflect.{ClassTag, classTag}








object Test {

  def main(args: Array[String]): Unit = {
    ConcurrentMapSpec.test()
    IteratorSpec.test()
    LNodeSpec.test()
    SnapshotSpec.test()
  }

}


trait Spec {

  implicit def implicitously: languageFeature.implicitConversions = scala.language.implicitConversions
  implicit def reflectively : languageFeature.reflectiveCalls = scala.language.reflectiveCalls

  implicit def str2ops(s: String): AnyRef{def in[U](body: => U): Unit} = new {
    def in[U](body: =>U): Unit = {
      // just execute body
      body
    }
  }

  implicit def any2ops(a: Any): AnyRef{def shouldEqual(other: Any): Unit} = new {
    def shouldEqual(other: Any) = assert(a == other)
  }

  def evaluating[U](body: =>U) = new {
    def shouldProduce[T <: Throwable: ClassTag]() = {
      var produced = false
      try body
      catch {
        case e: Throwable => if (e.getClass == implicitly[ClassTag[T]].runtimeClass) produced = true
      } finally {
        assert(produced, "Did not produce exception of type: " + implicitly[ClassTag[T]])
      }
    }
  }

}
