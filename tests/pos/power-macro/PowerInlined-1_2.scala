object PowerInlined1 {
  import PowerMacro._

  power(1L, 5.0) // 1 quotes to unpickle
}
