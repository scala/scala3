object Test {

  def main(args: Array[String]): Unit = {
    println(rewrite("foo"))
    println(rewrite("foo" + "foo"))

    rewrite {
      println("apply")
    }

    rewrite {
      println("block")
      println("block")
    }

    val b: Boolean = true
    rewrite {
      if b then println("then")
      else println("else")
    }

    rewrite {
      if !b then println("then")
      else println("else")
    }

    rewrite {
      val s: String = "val"
      println(s)
    }

    rewrite {
      val s: "vals" = "vals"
      println(s)
    }

    rewrite {
      def s: String = "def"
      println(s)
    }

    rewrite {
      def s: "defs" = "defs"
      println(s)
    }

    rewrite {
      def s(x: String): String = x
      println(s("def"))
    }

    rewrite {
      var s: String = "var"
      s = "bar"
      println(s)
    }

    rewrite {
      try println("try")
      finally println("finally")
    }

    rewrite {
      try throw new Exception()
      catch case x: Exception => println("catch")
    }

    rewrite {
      var x = true
      while (x) {
        println("while")
        x = false
      }
    }

    rewrite {
      val t = new Tuple1("new")
      println(t._1)
    }

    rewrite {
      println("typed": String)
      println("typed": Any)
    }

    rewrite {
      val f = new Foo(foo = "namedArg")
      println(f.foo)
    }

    rewrite {
      println("qual".reverse)
    }

    rewrite {
      val f = () => "lambda"
      println(f())
    }

    rewrite {
      def f(args: String*): String = args.mkString
      println(f("var", "args"))
    }

    rewrite {
      "match" match {
        case "match" => println("match")
        case x => println("x")
      }
    }

    // FIXME should print fed
    rewrite {
      def s: String = return "def"
      println(s)
    }

    rewrite {
      class Foo {
        println("new Foo")
      }
      new Foo
    }


  }

}

class Foo(val foo: String)
