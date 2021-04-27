package foo

trait TOops
{
    def oops: Unit ?=> Unit
}

trait Oops extends TOops
{
    override def oops: Unit ?=> Unit = (x) ?=> x
}
