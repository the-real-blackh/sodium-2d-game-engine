#!/bin/sh -e
rm rts.js lib1.js lib1.js.files lib.js lib.js.files out.js out.stats
uglifyjs -o all.min.js all.js
mv all.min.js all.js
