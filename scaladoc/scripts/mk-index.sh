#!/usr/bin/env bash

MYDIR="$(dirname "$(readlink -f "$0")")"


function html-beginning {
	cat << EOF
<html>
    <body>
EOF
}

function html-ending {
cat << EOF
	    </body>
</html>
EOF
}

function print-beginning {
	cat << EOF
        <p> $1 </p>
        <ul>
EOF
}

function print-list-element {
	cat << EOF
        <li> <a href="$1">$2</a> </li>
EOF
}

function print-list-element-text {
	cat << EOF
		<li> $1 </li>
EOF
}

function print-ending {
	cat << 'EOF'
        </ul>
EOF
}

cd "$1" || exit


html-beginning
print-beginning "Available project:"
for f in *
do
	! [[ -d $f ]] && continue
	# assuming that there's only one "root" index file
	# if there's a static site, it's at depth 1
	# otherwise at depth 2
	INDEX=$(find "$f" -maxdepth 1 -name 'index.html')
	if [[ -z $INDEX ]]
	then
		INDEX=$(find "$f" -maxdepth 2 -name 'index.html')
	fi
	print-list-element "$INDEX" "$f"
done
print-ending

print-beginning "Links for manual testing:"
while read line; do
	print-list-element-text "$line"
done < "$MYDIR/tocheck.txt"

while read line; do
	IFS=',' read INDEX f <<< "${line}"
	print-list-element "$INDEX" "$f"
done < "$MYDIR/linkstovisit.txt"
print-ending
html-ending
