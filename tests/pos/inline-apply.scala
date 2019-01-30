class Context

object Test {

  def transform()(implicit ctx: Context) = {
    inline def withLocalOwner[T](op: given Context => T) = op given ctx

    withLocalOwner { given ctx => }

  }
}
