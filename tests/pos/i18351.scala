package test1:
  class Service(val name: String)
  class CrudService(name: String) extends Service(name)

  trait Foo { self: CrudService =>
    val x = self.name
  }

package test2:
  abstract class Service[F[_]](val name: String)
  abstract class CrudService[F[_]](name: String) extends Service[F](name)

  trait Foo[F[_]] { self: CrudService[?] =>
    val x = self.name
  }
