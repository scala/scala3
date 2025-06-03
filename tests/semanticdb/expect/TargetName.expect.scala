package example

object TargetName/*<-example::TargetName.*/:
  @annotation.targetName/*->scala::annotation::targetName#*/("m1")
  def m/*<-example::TargetName.m().*/(i/*<-example::TargetName.m().(i)*/: Int/*->scala::Int#*/) = 1
  @annotation.targetName/*->scala::annotation::targetName#*/("m2")
  def m/*<-example::TargetName.m(+1).*/(i/*<-example::TargetName.m(+1).(i)*/: Int/*->scala::Int#*/) = 1
  def m1/*<-example::TargetName.m1().*/(i/*<-example::TargetName.m1().(i)*/: String/*->scala::Predef.String#*/) = 1
