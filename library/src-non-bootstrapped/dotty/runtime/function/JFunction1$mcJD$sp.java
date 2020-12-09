
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcJD$sp extends JFunction1<Object, Object> {
    abstract long apply$mcJD$sp(double v1);

    default Object apply(Object t) { return (Long) apply$mcJD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)); }
}
