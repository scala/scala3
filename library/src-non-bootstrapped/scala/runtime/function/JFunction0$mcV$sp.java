
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.function;

@FunctionalInterface
public interface JFunction0$mcV$sp extends JFunction0 {
    abstract void apply$mcV$sp();

    default Object apply() { apply$mcV$sp(); return scala.runtime.BoxedUnit.UNIT; }
}
