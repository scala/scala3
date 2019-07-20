object Test {
  import compiletime.memo
  val a = memo(1) // error: memo(...) outside method
}