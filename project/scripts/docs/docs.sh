#!/bin/bash
INPUT=./project/scripts/docs/docs.csv
OLDIFS=$IFS
IFS=','
[ ! -f $INPUT ] && { echo "$INPUT file not found"; exit 99; }

echo '{"versions":{'

while read label ref
do
	# echo "\"$label\": \"https://dotty.epfl.ch/$ref/docs/index.html\",",
	echo "\"$label\": \"http://127.0.0.1:5500/docs/_site/$ref/docs/index.html\","
done < $INPUT
IFS=$OLDIFS
# echo '"Nightly": "https://dotty.epfl.ch/docs/index.html",'
echo '"Nightly": "http://127.0.0.1:5500/docs/_site/docs/index.html"'
echo '}}'

