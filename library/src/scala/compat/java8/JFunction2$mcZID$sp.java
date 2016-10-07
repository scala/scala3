
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction2$mcZID$sp extends JFunction2 {
    abstract boolean apply$mcZID$sp(int v1, double v2);

    default Object apply(Object v1, Object v2) { return (Boolean) apply$mcZID$sp((Integer) v1, (Double) v2); }
}
