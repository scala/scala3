class Context

object Test {

  def transform()(implicit ctx: Context) = {
    transparent def withLocalOwner[T](op: Context => T) = op(ctx)

    withLocalOwner { implicit ctx => }

  }
}
