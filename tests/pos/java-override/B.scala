//trait T { def t(o: Object): Unit }

class B extends A /*with T*/ {
  override def o(o: Any): Unit = ()

  //override def t(o: AnyRef): Unit = ()
}
