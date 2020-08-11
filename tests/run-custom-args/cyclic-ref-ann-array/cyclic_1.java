import java.lang.annotation.*;

@interface BaseClassAnn {
    Type[] value();
    @interface Type {
        Class<?> value();
    }
}

@BaseClassAnn({
    @BaseClassAnn.Type(value=DerivedClass.class)
})
abstract class BaseClass {}

class DerivedClass extends BaseClass {}
