
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction1$mcVI$sp extends JFunction1 {
    abstract void apply$mcVI$sp(int v1);

    default Object apply(Object t) { apply$mcVI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)); return scala.runtime.BoxedUnit.UNIT; }
}
