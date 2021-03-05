package ext

/*<-ext::Extension$package.*/extension (s/*<-ext::Extension$package.foo().(s)*//*<-ext::Extension$package.`#*#`().(s)*/: String/*->scala::Predef.String#*/)
  def foo/*<-ext::Extension$package.foo().*/: Int/*->scala::Int#*/ = 42
  def #*#/*<-ext::Extension$package.`#*#`().*/ (i/*<-ext::Extension$package.`#*#`().(i)*/: Int/*->scala::Int#*/): (String/*->scala::Predef.String#*/, Int/*->scala::Int#*/) = (/*->scala::Tuple2.apply().*/s/*->ext::Extension$package.`#*#`().(s)*/, i/*->ext::Extension$package.`#*#`().(i)*/)

val a/*<-ext::Extension$package.a.*/ = "asd".foo/*->ext::Extension$package.foo().*/

val c/*<-ext::Extension$package.c.*/ = "foo" #*#/*->ext::Extension$package.`#*#`().*/ 23