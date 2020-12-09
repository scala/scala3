
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcID$sp extends JFunction1<Object, Object> {
    abstract int apply$mcID$sp(double v1);

    default Object apply(Object t) { return (Integer) apply$mcID$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)); }
}
