object Test extends dotty.runtime.LegacyApp{
  Array[String]() match {
    case x@Array() => println(x.deep.toString());
  }
}
