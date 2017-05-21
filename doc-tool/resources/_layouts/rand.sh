currdate=$(date "+%Y-%m-%d\ %H:%M:%S")
currDateComment="<!-- Docs Last Generated On "$currdate"-->"
sed -i '.original' "1s/^/$currDateComment\\
/" main.html
