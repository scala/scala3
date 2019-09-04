object Test extends App{
  Array[String]() match {
    case x@Array() => println(x.toList.toString());
  }
}
