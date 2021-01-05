def usered = Color.Red // error: value Red is deprecated

enum Color {
  @deprecated("no Red", "0.1") case Red
}
