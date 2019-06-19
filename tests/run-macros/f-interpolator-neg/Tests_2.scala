object Test {

	  def main(args: Array[String]): Unit = {
      val s = "Scala"
      val d = 8
      val b = false
      val f = 3.14159
      val c = 'c'
      val t = new java.util.Date
      val x = new java.util.Formattable {
        def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff format "xxx"
      }
	    numberArgumentsTests(s, d)
      interpolationMismatches(s, f, b)
      flagMismatches(s, c, d, f, t)
      badPrecisions(c, d, f, t)
      badIndexes()
      warnings(s)
      badArgTypes(s)
      misunderstoodConversions(t, s)
      otherBrainFailures(d)
	  }

    def numberArgumentsTests(s : String, d : Int) = {
      import TestFooErrors._
      assertEquals(new StringContext().foo(), List((true, 2, -1, 0, "there are no parts")))
      assertEquals(new StringContext("", " is ", "%2d years old").foo(s), List((true, 1, 0, 0, "too few arguments for interpolated string")))
      assertEquals(new StringContext("", " is ", "%2d years old").foo(s, d, d), List((true, 1, 2, 0, "too many arguments for interpolated string")))
      assertEquals(new StringContext("", "").foo(), List((true, 3, -1, 0, "too few arguments for interpolated string")))
    }

    def interpolationMismatches(s : String, f : Double, b : Boolean) = {
      import TestFooErrors._
      assertEquals(foo"$s%b", List((true, 1, 0, 0, "type mismatch;\n found   : String\n required: Boolean")))
      assertEquals(foo"$s%c", List((true, 1, 0, 0, "type mismatch;\n found   : String\n required: Char")))
      assertEquals(foo"$f%c", List((true, 1, 0, 0, "type mismatch;\n found   : Double\n required: Char")))
      assertEquals(foo"$s%x", List((true, 1, 0, 0, "type mismatch;\n found   : String\n required: Int")))
      assertEquals(foo"$b%d", List((true, 1, 0, 0, "type mismatch;\n found   : Boolean\n required: Int")))
      assertEquals(foo"$s%d", List((true, 1, 0, 0, "type mismatch;\n found   : String\n required: Int")))
      assertEquals(foo"$f%o", List((true, 1, 0, 0, "type mismatch;\n found   : Double\n required: Int")))
      assertEquals(foo"$s%e", List((true, 1, 0, 0, "type mismatch;\n found   : String\n required: Double")))
      assertEquals(foo"$b%f", List((true, 1, 0, 0, "type mismatch;\n found   : Boolean\n required: Double")))
      assertEquals(foo"$s%i", List((true, 0, 1, 1, "illegal conversion character 'i'")))
    }

    def flagMismatches(s : String, c : Char, d : Int, f : Double, t : java.util.Date) = {
      import TestFooErrors._
      assertEquals(foo"$s%+ 0,(s", List((true, 0, 1, 1, "Illegal flag '+'"), (true, 0, 1, 2, "Illegal flag ' '"),
        (true, 0, 1, 3, "Illegal flag '0'"), (true, 0, 1, 4, "Illegal flag ','"), (true, 0, 1, 5, "Illegal flag '('")))
      assertEquals(foo"$c%#+ 0,(c", List((true, 0, 1, 1, "Only '-' allowed for c conversion")))
      assertEquals(foo"$d%#d", List((true, 0, 1, 1, "# not allowed for d conversion")))
      assertEquals(foo"$d%,x", List((true, 0, 1, 1, "',' only allowed for d conversion of integral types")))
      assertEquals(foo"$d%+ (x", List((true, 0, 1, 1, "only use '+' for BigInt conversions to o, x, X"), (true, 0, 1, 2, "only use ' ' for BigInt conversions to o, x, X"),
        (true, 0, 1, 3, "only use '(' for BigInt conversions to o, x, X")))
      assertEquals(foo"$f%,(a", List((true, 0, 1, 1, "',' not allowed for a, A"), (true, 0, 1, 2, "'(' not allowed for a, A")))
      assertEquals(foo"$t%#+ 0,(tT", List((true, 0, 1, 1, "Only '-' allowed for date/time conversions")))
      assertEquals(foo"%-#+ 0,(n", List((true, 0, 0, 1, "flags not allowed")))
      assertEquals(foo"%#+ 0,(%", List((true, 0, 0, 1, "Illegal flag '#'"), (true, 0, 0, 2, "Illegal flag '+'"),
        (true, 0, 0, 3, "Illegal flag ' '"), (true, 0, 0, 4, "Illegal flag '0'"), (true, 0, 0, 5, "Illegal flag ','"), (true, 0, 0, 6, "Illegal flag '('")))
    }

    def badPrecisions(c : Char, d : Int, f : Double, t : java.util.Date) = {
      import TestFooErrors._
      assertEquals(foo"$c%.2c", List((true, 0, 1, 1, "precision not allowed")))
      assertEquals(foo"$d%.2d", List((true, 0, 1, 1, "precision not allowed")))
      assertEquals(foo"%.2%", List((true, 0, 0, 1, "precision not allowed")))
      assertEquals(foo"%.2n", List((true, 0, 0, 1, "precision not allowed")))
      assertEquals(foo"$f%.2a", List((true, 0, 1, 1, "precision not allowed")))
      assertEquals(foo"$t%.2tT", List((true, 0, 1, 1, "precision not allowed")))
    }

    def badIndexes() = {
      import TestFooErrors._
      assertEquals(foo"%<s", List((true, 0, 0, 1, "No last arg")))
      assertEquals(foo"%<c", List((true, 0, 0, 1, "No last arg")))
      assertEquals(foo"%<tT", List((true, 0, 0, 1, "No last arg")))
      assertEquals(foo"${8}%d ${9}%d %3$$d", List((true, 0, 2, 4, "Argument index out of range")))
      assertEquals(foo"${8}%d ${9}%d%0$$d", List((true, 0, 2, 3, "Argument index out of range")))
    }

    def warnings(s : String) = {
      import TestFooErrors._
      assertEquals(foo"${8}%d ${9}%1$$d", List((false, 0, 2, 1, "Index is not this arg")))
      assertEquals(foo"$s%s $s%s %1$$<s", List((false, 0, 2, 4, "Argument index ignored if '<' flag is present")))
      assertEquals(foo"$s%s $s%1$$s", List((false, 0, 2, 1, "Index is not this arg")))
    }

    def badArgTypes(s : String) = {
      import TestFooErrors._
      assertEquals(foo"$s%#s", List((true, 1, 0, 0, "type mismatch;\n found   : String\n required: java.util.Formattable")))
    }

    def misunderstoodConversions(t : java.util.Date, s : String) = {
      import TestFooErrors._
      assertEquals(foo"$t%tG", List((true, 0, 1, 2, "'G' doesn't seem to be a date or time conversion")))
      assertEquals(foo"$t%t", List((true, 0, 1, 1, "Date/time conversion must have two characters")))
      assertEquals(foo"$s%10.5", List((true, 0, 1, 0, "Missing conversion operator in '%10.5'; use %% for literal %, %n for newline")))
    }

    def otherBrainFailures(d : Int) = {
      import TestFooErrors._
      assertEquals(foo"${d}random-leading-junk%d", List((true, 0, 1, 19, "conversions must follow a splice; use %% for literal %, %n for newline")))
      assertEquals(StringContext().foo(), List((true, 2, -1, 0, "there are no parts")))
      assertEquals(foo"%1$$n", Nil)
      assertEquals(foo"%1$$d", List((true, 0, 0, 1, "Argument index out of range")))
      assertEquals(foo"blablablabla %% %.2d", List((true, 0, 0, 17, "precision not allowed")))
      assertEquals(foo"blablablabla %.2b %%", List((true, 0, 0, 13, "conversions must follow a splice; use %% for literal %, %n for newline")))

      assertEquals(foo"ana${3}%.2foo%2${true}%bb", List((true, 0, 1, 6, "Missing conversion operator in '%2'; use %% for literal %, %n for newline")))
      assertEquals(foo"ac{2c{2{c.ca ", Nil)
      assertEquals(foo"b%c.%2ii%iin", List((true, 0, 0, 1, "conversions must follow a splice; use %% for literal %, %n for newline"),
        (true, 0, 0, 6, "illegal conversion character 'i'"), (true, 0, 0, 9, "illegal conversion character 'i'")))
      assertEquals(foo"b}22%2.c<{%{" , List((true, 0, 0, 4, "Missing conversion operator in '%2'; use %% for literal %, %n for newline"),
        (true, 0, 0, 10, "Missing conversion operator in '%'; use %% for literal %, %n for newline")))
      assertEquals(foo"%%bci.2${'i'}%..2c2", List((true, 0, 1, 0, "Missing conversion operator in '%'; use %% for literal %, %n for newline")))
    }

	  def assertEquals(actual: Any, expected: Any): Unit = {
	    assert(actual == expected, s"actual: $actual\nbut expected: $expected")
	  }
	}