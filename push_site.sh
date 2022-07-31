#!/bin/sh -x

set -e

commit_id=`git log -1 --format=format:"%h: %s (%ai)"`

cd _site
if [ ! -d .git ]; then
    git init

    # reuse local checkout's objects if possible
    alternate_store=$(pwd)/../../aleph_null/.git/objects
    if [ ! -d ${alternate_store} ]; then
        echo "WARNING: '$alternate_store' checkout does not exist"
        echo "WARNING: push will always refetch initial state."
    else
        echo "$alternate_store" >> .git/objects/info/alternates
    fi

    git checkout --orphan gh-pages
    git remote add --fetch origin git@github.com:skvadrik/aleph_null.git
fi
git add .
git commit -s -m "sync skvadrik.github.io/aleph_null.git:${commit_id}" .
git push --force -u origin gh-pages
