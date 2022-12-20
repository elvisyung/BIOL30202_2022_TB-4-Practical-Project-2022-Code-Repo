<<com
Grab files with only 2 blastp hits and moves it elsewhere
com
for file in *
do awk 'NR==3{exit}END{exit NR==2}' "$file" || cp "$file" /path/
done