
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcDJ$sp extends JFunction1<Object, Object> {
    abstract double apply$mcDJ$sp(long v1);

    default Object apply(Object t) { return (Double) apply$mcDJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)); }
}
