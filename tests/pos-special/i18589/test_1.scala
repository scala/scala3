enum Data {
  case A, B, C
}

@main def Test = {
  val builder: NamedCodecPlatform.Builder[Any] = ???
  builder.of[Data]
}
