echo "// Generated from singleton-ops-long-normalize.scala by singleton-ops-int-normalize_make.sh" > singleton-ops-int-normalize.scala

sed -E -e "s/([0-9]+)L/\1/g" -e "s/Long/Int/g" -e "s/long/int/g" singleton-ops-long-normalize.scala >> singleton-ops-int-normalize.scala
