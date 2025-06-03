import java.util.concurrent.TimeUnit
@MyAnnotation() class Test1
@MyAnnotation(TimeUnit.DAYS) class Test2
@MyAnnotation(TimeUnit.DAYS, TimeUnit.DAYS) class Test3
@MyAnnotation(TimeUnit.DAYS, TimeUnit.DAYS, "foo") class Test4
@MyAnnotation(TimeUnit.DAYS, TimeUnit.DAYS, "foo", "bar") class Test5