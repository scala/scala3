package example

class EtaExpansion/*<-example::EtaExpansion#*/ {
  Some/*->scala::Some.*/(1).map/*->scala::Option#map().*/(identity/*->scala::Predef.identity().*/)
  List/*->scala::package.List.*/(1).foldLeft/*->scala::collection::LinearSeqOps#foldLeft().*/("")(_ +/*->java::lang::String#`+`().*/ _)
}
