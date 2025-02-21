#!/usr/bin/env zsh
COOKIE=
for task in {49..73}; do
  id=$((task+656))
  curl 'https://autotool.fmi.uni-due.de/route/aufgabe/'${id}'/instanz' -o ${task}.hs --compressed -H "${COOKIE}"
  curl 'https://autotool.fmi.uni-due.de/route/aufgabe/'${id} --silent --compressed -H "${COOKIE}" | sed -n 's/.*<label for="hident127">\([^<]*\)<.*/\1/p' > "${task}.txt"
done
