
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcVJ$sp extends JFunction1<Object, Object> {
    abstract void apply$mcVJ$sp(long v1);

    default Object apply(Object t) { apply$mcVJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)); return scala.runtime.BoxedUnit.UNIT; }
}
