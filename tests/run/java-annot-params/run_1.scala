package annots

def runTest(cls: Class[_]): Unit =
  val params =
    Option(cls.getAnnotation(classOf[WithClass])).map(_.arg) ::
    Option(cls.getAnnotation(classOf[WithClassDefaultName])).map(_.value) ::
    Option(cls.getAnnotation(classOf[WithString])).map(_.arg) ::
    Option(cls.getAnnotation(classOf[WithReference])).map(_.arg) ::
    Option(cls.getAnnotation(classOf[WithBoolean])).map(_.arg) ::
    Option(cls.getAnnotation(classOf[WithFloat])).map(_.arg) ::
    Option(cls.getAnnotation(classOf[WithNested])).map(_.arg.value) ::
    Option(cls.getAnnotation(classOf[WithArray])).map(_.value.toList) ::
    Option(cls.getAnnotation(classOf[WithEmptyArray])).map(_.value.toList) ::
    Option(cls.getAnnotation(classOf[WithSingleElement])).map(_.value.toList) ::
    Option(cls.getAnnotation(classOf[WithMultipleArgs])).map(_.value.toList) ::
    Nil
  params.flatten.foreach(println)