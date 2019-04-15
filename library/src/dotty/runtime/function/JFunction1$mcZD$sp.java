
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcZD$sp extends JFunction1<Object, Object> {
    abstract boolean apply$mcZD$sp(double v1);

    default Object apply(Object t) { return (Boolean) apply$mcZD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)); }
}
