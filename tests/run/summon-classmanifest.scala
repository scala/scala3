import scala.reflect.{ClassManifest, ClassTag}

type CM[T] = reflect.ClassManifest[T] @annotation.nowarn("msg=deprecated")
type CManifest[T] = CM[T] // test dealiasing mixed with annotated types

@main def Test =
  // manifests are ClassTags
  val manifestListOptionInt: ClassTag[List[Option[Int]]] = summon[CManifest[List[Option[Int]]]]
  val manifestArrayString: ClassTag[Array[String]] = summon[CManifest[Array[String]]]

  println(manifestListOptionInt) // should print arguments to List
  println(manifestArrayString)
