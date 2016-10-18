
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction0$mcZ$sp extends JFunction0 {
    abstract boolean apply$mcZ$sp();

    default Object apply() { return (Boolean) apply$mcZ$sp(); }
}
