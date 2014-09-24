#!/bin/sh

set -e

commit_id=`git log -1 --format=format:"%h: %s (%ai)"`

cd _site
if [ ! -d .git ]; then
    git init
    git checkout --orphan gh-pages
    git remote add --fetch origin git@github.com:skvadrik/aleph_null.git
fi
git add .
git commit -s -m "sync skvadrik.github.io/aleph_null.git:${commit_id}" .
git push --force -u origin gh-pages
