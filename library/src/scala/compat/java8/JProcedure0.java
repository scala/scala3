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
public interface JProcedure0 extends JFunction0<BoxedUnit> {
    default void $init$() {
    }

    void applyVoid();

    default BoxedUnit apply() {
        applyVoid();
        return BoxedUnit.UNIT;
    }
}
