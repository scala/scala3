#!/usr/bin/env bash

function print-beginning {
	cat << 'EOF'
<html>
    <body>
        <p> Available projects: </p>
        <ul>
EOF
}

function print-list-element {
	cat << EOF
        <li> <a href="$1">$2</a> </li>
EOF
}

function print-ending {
	cat << 'EOF'
        </ul>
    </body>
</html>
EOF
}

cd "$1" || exit

print-beginning
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
