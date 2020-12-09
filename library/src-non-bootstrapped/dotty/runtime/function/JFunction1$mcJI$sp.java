
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcJI$sp extends JFunction1<Object, Object> {
    abstract long apply$mcJI$sp(int v1);

    default Object apply(Object t) { return (Long) apply$mcJI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)); }
}
