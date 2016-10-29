#!/bin/zsh
mkdir -p ~/tmp/
cd ~/tmp
wget -l 0 --html-extension --execute robots=off --mirror --convert-links --adjust-extension --page-requisites --no-parent http://eris:3000/koeppldo
cd eris:3000
rm -r static/video auth static/bin/gen/
for i in **/*\?etag*; do mv "$i" $(echo $i | sed 's@\?etag.*@@'); done
find -name "*.html" -exec sed -i 's@?etag=[a-zA-Z0-9_\-\.-]\+@@g' {} \;
find -name "*.html" -exec sed -i 's@%3Fetag=[a-zA-Z0-9_\-\.-]\+@@g' {} \;
