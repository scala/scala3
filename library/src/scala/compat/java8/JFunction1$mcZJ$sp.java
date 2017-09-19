
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction1$mcZJ$sp extends JFunction1 {
    abstract boolean apply$mcZJ$sp(long v1);

    default Object apply(Object t) { return (Boolean) apply$mcZJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)); }
}
