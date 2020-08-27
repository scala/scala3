trait JsonSchemas {
  locally {
    sealed trait Status
    case object Active extends Status
  }
}
