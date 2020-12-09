
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure1<T1> extends JFunction1<T1, BoxedUnit>, java.io.Serializable {
    void applyVoid(T1 t1);

    default BoxedUnit apply(T1 t1) {
        applyVoid(t1);
        return BoxedUnit.UNIT;
    }
}
