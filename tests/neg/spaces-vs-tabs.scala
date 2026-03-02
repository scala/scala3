object Test:

	if true then
		println(1)   // ok
    println(2)   // error
  	println(3)   // error
	  println(4)   // error
 else ()         // error

	object Test2:

	    if true then
	      ()
	  else 2 // error

