#!/bin/bash
# Basically runs through a number of git commands and creates a fairly
# thick, complex Git repository, that is in a reliable known state.
# Of course, I could easily create this once, and then backup the .git
# directory...

BASE=${1:-"/tmp/sandbox"}

PROJECT=my-proj
OTHER=.other-$PROJECT

DAY=13    # The number of days 'back' to add commits. Makes the log more believable

# Deletes and reinitializes all of three git repositories ...
#   1. The primary cloned project
#   2. The "other" cloned project for submitting conflicts
#   3. The "remote" directory that acts like a server

function initialize {
    PRJ=${1:-$PROJECT}
    BSE=${2:-$BASE}
    OTH=${3:-$OTHER}

    rm -rf $BSE/.remote-repos/$PRJ
    rm -rf $BSE/$PRJ
    rm -rf $BSE/$OTH

    # Get the remote repository ready ...
    mkdir -p $BSE/.remote-repos/$PRJ
    cd $BSE/.remote-repos/$PRJ
    git init --bare

    mkdir -p $BSE/$PRJ
    cd $BSE/$PRJ
    git init
    git remote add origin $BSE/.remote-repos/$PRJ

    # We can simply clone this guy later...
    # mkdir -p $BSE/$OTH
    # cd $BSE/$OTH
    # git init
    # git remote add origin $BSE/.remote-repos/$PRJ
}

# This function adds a 'Change' item with an incrementing number, to
# make it easier to tell which commit went with what change. The
# optional third parameter can specify the item label, which defaults
# to "Change".

function append {
    FILE=$1
    TEXT=$2
    ITEM=${3:-"Change"}
    ID=$(grep "$ITEM " $FILE | tail -1 | perl -ne 'print $1 if (/#([0-9]+):/)')

    if [ -z "$ID" ]
    then
        ID=1
    else
        ID=$(expr $ID + 1)
    fi
    echo "$ITEM #$ID: $TEXT" >> $FILE
    echo
}

function commit {
    OPT=""
    if [ -e "$1" ]
    then
        git add $1
        shift
    else
        OPT="-a"
    fi

    if [ $DAY -gt 0 ]
    then
        git commit $OPT --date "-$DAY day" -m "$*"
        DAY=$(expr $DAY - 1)
    else
        git commit $OPT -m "$*"
    fi
}

# ----------------------------------------------------------------------
# Create the local repository
initialize $PROJECT $BASE

# Now let's get working on the repo history...
cd $BASE/$PROJECT

echo "This is a README about this project" > README.txt
echo >> README.txt
commit README.txt "Initial commit."

append README.txt "I really need to say something important."
commit README.txt "Update readme message"

append README.txt "What would you add?"
commit README.txt "Further vaccuous readme update"

append README.txt "Probably something like this. ;-)"
commit README.txt "Yet another superfluous readme update"

git push origin master
git branch --set-upstream-to=origin/master master

git checkout -b side-project
append README.txt "At this point, lorem ipsum looks good."
commit README.txt "Trying to be humorous"

append README.txt "The fifth doctor was the best."
commit README.txt "Start a flame war I can't join"

git checkout -b side-side-project
append README.txt "Now we are getting interesting."
commit README.txt "These branches are really far afield"

append README.txt "Legolas is really a dwarven robot invention."
commit README.txt "Final change for ultimate domination"

git checkout side-project
append README.txt "How about a little conflict?"
commit README.txt "Integrating feedback from user group"

# git merge side-side-project
# git commit -a -m "Closing the side-side project"

# Time for the conflict ... first of many...
perl -pi -e "s/Legolas/Gimli/" README.txt
commit README.txt "Integrated the diversion"

append README.txt "C.S. Lewis was boring."
commit README.txt "Next step in listing silly controversies"

git checkout master
append README.txt "Which is correct?"
commit README.txt "Time for merge conflicts."

append README.txt "Shouldn't we just take them all?"
commit README.txt "Edits based on user feedback."

git push origin master


# ----------------------------------------------------------------------
# In order to simulate other people checking in code, we need to
# create a separate cloned repository.

git clone $BASE/.remote-repos/$PROJECT $BASE/$OTHER
cd $BASE/$OTHER

append README.txt "Murdered by pirates is good." "Other"
git commit -a  --date '-2 day' --author "Arnold Rimmer <arimmer@hologramsrus.com>" -m "Appears we forgot a part"

append README.txt "Never go against a Sicilian when death is on the line!" "Other"
git commit -a --date '-1 day'  --author "Dave Lister <dlist@fuzzytoast.com>" -m "Seems we've forgotten two parts"

git push origin master

# ----------------------------------------------------------------------
# Create a file and stage it so that it can be sitting in the hopper.

cd $BASE/$PROJECT

append README.txt "Version history should go here."
perl -pi -e 's/^Change #2:.*$/Change #2: What would you remove?/' README.txt
perl -pi -e 's/^This/qThis/' README.txt

cat > LICENSE.txt <<EOF
This is a license to thrill. Bad puns are what makes a dad, a dad.
EOF
git add LICENSE.txt


# Create some files that need to be staged and committed.
# Two files should be sufficient:

cat > CONTRIBUTING.md <<EOF
Contributions are always welcome, no matter how large or small. Here
are the guidelines we ask our contributors to follow:

- For any contributions to our '2.0' track, please create a topic
  branch based off our 'master' branch, push said topic branch onto
  your fork and submit your pull request from that branch.

- Anyone wishing to contribute a bug fix to our '1.x' track, please
  create a topic branch based off our '1.x' branch, push said topic
  branch onto your fork and submit your pull request from that branch.
EOF

cat > CODING_RULES.md <<EOF
For simplicity, we've divided our coding rules using the same
categories as the ESLint documentation.

- [Possible Errors](#errors)
- [Best Practices](#best)
- [Strict Mode](#strict)
- [Variables](#variables)
- [Node.js](#node)
- [Stylistic Issues](#style)
EOF

# Create a file that we want to add to the .gitignore...
# ctags -L *
touch tags

# Create a file that must have been left over to be deleted.
touch debug-output.txt

echo
echo "All done. Checkout the goodies:   $BASE/$PROJECT"
