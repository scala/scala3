
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcJF$sp extends JFunction1<Object, Object> {
    abstract long apply$mcJF$sp(float v1);

    default Object apply(Object t) { return (Long) apply$mcJF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); }
}
