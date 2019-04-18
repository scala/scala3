
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcIF$sp extends JFunction1<Object, Object> {
    abstract int apply$mcIF$sp(float v1);

    default Object apply(Object t) { return (Integer) apply$mcIF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); }
}
