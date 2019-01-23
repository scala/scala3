/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure2<T1, T2> extends JFunction2<T1, T2, BoxedUnit> {
    default void $init$() {
    }

    void applyVoid(T1 t1, T2 t2);

    default BoxedUnit apply(T1 t1, T2 t2) {
        applyVoid(t1, t2);
        return BoxedUnit.UNIT;
    }
}
