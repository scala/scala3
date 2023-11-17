package org.jooq.impl;

import org.jooq.Configuration;
import org.jooq.Attachable;

public abstract class AbstractRoutine<T> extends AbstractQueryPart implements Attachable {
  @Override
  public final Configuration configuration() {
    return null;
  }
}
