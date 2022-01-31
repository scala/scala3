echo "// Generated from singleton-ops-long-normalize.scala by singleton-ops-int-normalize_make.sh" > singleton-ops-int-normalize.scala

head -97 singleton-ops-long-normalize.scala | \
  sed -E -e "s/([0-9]+)L/\1/g" -e "s/Long/Int/g" -e "s/long/int/g" >> singleton-ops-int-normalize.scala
