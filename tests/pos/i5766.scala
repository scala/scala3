object Test1 {
  trait Foo { def next: Foo }

  inline implicit def foo(implicit loop: => Foo): Foo = new Foo { def next = loop }

  def summon(implicit f: Foo) = ()
  summon
}

object Test2 {
  trait Foo { def next: Bar }
  trait Bar { def next: Foo }

  implicit def foo(implicit loop: => Bar): Foo = new Foo { def next = loop }
  inline implicit def bar(implicit loop: Foo): Bar = new Bar { def next = loop }

  def summon(implicit f: Foo) = ()
  summon
}

object Test3 {
  trait Foo { def next: Bar }
  trait Bar { def next: Baz }
  trait Baz { def next: Foo }

  implicit def foo(implicit loop: Bar): Foo = new Foo { def next = loop }
  inline implicit def bar(implicit loop: Baz): Bar = new Bar { def next = loop }
  inline implicit def baz(implicit loop: => Foo): Baz = new Baz { def next = loop }

  def summon(implicit f: Foo) = ()
  summon
}
