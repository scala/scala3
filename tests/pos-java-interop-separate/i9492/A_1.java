import java.lang.annotation.*;

@interface BaseClassAnn {
    Type[] value();
    @interface Type {
        Class<?> value();
    }
}

@BaseClassAnn({
    @BaseClassAnn.Type(value=A_1.class)
})
abstract class BaseClass {}

public class A_1 extends BaseClass {}
