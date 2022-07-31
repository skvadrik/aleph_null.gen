#!/bin/sh -x

set -e

commit_id=`git log -1 --format=format:"%h: %s (%ai)"`

cd _site
if [ ! -d .git ]; then
    git init --initial-branch=gh-pages

    # reuse local checkout's objects if possible
    alternate_store=$(pwd)/../../aleph_null/.git/objects
    if [ ! -d ${alternate_store} ]; then
        echo "WARNING: '$alternate_store' checkout does not exist"
        echo "WARNING: push will always refetch initial state."
    else
        echo "$alternate_store" >> .git/objects/info/alternates
    fi

    git remote add --fetch origin git@github.com:skvadrik/aleph_null.git
    # Use pristine freshly generated _site state for new commit.
    # Don't take anything from parent except for the commit ID itself.
    git reset --soft origin/gh-pages
fi
git add .
git commit -s -m "sync skvadrik.github.io/aleph_null.git:${commit_id}" .
git push -u origin gh-pages
