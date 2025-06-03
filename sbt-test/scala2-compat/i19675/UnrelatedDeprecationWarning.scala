import com.twitter.finagle.Thrift
import com.twitter.finagle.thrift.ThriftService
import scala.reflect.ClassTag

class Minim {
  trait Foo[A]
  
  object Foo {
    inline def make[A]: Foo[A] = ???
  }

  final class Unrelated()

  object Unrelated {
    val foo = Foo.make[Unrelated]
  }

  object Main {
    def foo[S <: ThriftService](using ClassTag[S]) = 
      Thrift.client.build[S]("asd")
  }
}
