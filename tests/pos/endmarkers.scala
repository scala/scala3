trait T:
  object O:
    def foo =
      1
    end foo
    def bar = 2
    end bar
  end O
end T

object T
end T

package p1.p2:

  abstract class C():

    def this(x: Int) =
      this()
      if x > 0 then
        val a :: b =
          x :: Nil
        end val
        var y =
          x
        end y
        while y > 0 do
          println(y)
          y -= 1
        end while
        try
          x match
            case 0 => println("0")
            case _ =>
          end match
        finally
          println("done")
        end try
      end if
    end this

    def f: String
  end C

  object C:
    given C =
      new C:
        def f = "!"
        end f
      end new
    end given
  end C

  extension (x: C)
    def ff: String = x.f
  end extension

end p2