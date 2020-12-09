
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcJJ$sp extends JFunction1<Object, Object> {
    abstract long apply$mcJJ$sp(long v1);

    default Object apply(Object t) { return (Long) apply$mcJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)); }
}
