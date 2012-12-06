package dotty.tools.dotc.core

object periods {
  
  class A[T] { def m: T }
  class B { def m: Integer };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(131); 
  
  val x: A[String] & B;System.out.println("""x: => <error> = """ + $show(x));$skip(72); val res$0 = 
  
  x.m: AndRef(MemberRef(A.m, String), Symbol(B.m)): String & Integer;System.out.println("""res0: <error> = """ + $show(res$0));$skip(73); val res$1 = 
  
  if (x.isInstanceOf[A]) x.asInstanceOf[A].m else x.asInstanceOf[B].m;System.out.println("""res1: <error> = """ + $show(res$1))}
  
}
