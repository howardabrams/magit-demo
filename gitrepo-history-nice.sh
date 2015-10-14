#!/bin/bash
# Basically runs through a number of git commands and creates a fairly
# thick, complex Git repository, that is in a reliable known state.
#
# The basis of this project is a "quotes" files... just to be a bit
# more interesting.

BASE="/tmp/sandbox"

PROJECT=da-quotes
OTHER=.other-$PROJECT

DAY=7    # The number of days 'back' to add commits. Makes the log more believable

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
    git branch --set-upstream-to=origin/master master

    # We can simply clone this guy later...
    # mkdir -p $BSE/$OTH
    # cd $BSE/$OTH
    # git init
    # git remote add origin $BSE/.remote-repos/$PRJ
}

# This both adds quote to the quote file where the first parameter is
# the "source" (author or movie, whatnot).
function addquote {
    C=$1
    shift
    for Q in "$*"
    do
        echo "$Q" >> quotes.txt
    done

    if [ $DAY -gt 0 ]
    then
        DOPT="-$DAY day"
        DAY=$(expr $DAY - 1)
    fi

    auth[0]="John Cleese <john@cheeseshop.com>"
    auth[1]="Graham Chapman <oldchap@beyondthepale.org>"
    auth_num=$[ $RANDOM % 4 ]
    if [ $auth_num -lt 2 ]
    then
        AOPT="${auth[$auth_num]}"
    fi

    arr[0]="Quote"
    arr[1]="Another quote"
    arr[2]="Found something"
    arr[3]="Added quote"
    rand=$[ $RANDOM % 4 ]
    MSG="${arr[$rand]} from $C"

    git add quotes.txt
    if [ -n "$DOPT" ]
    then
        if [ -n "$AOPT" ]
        then
            git commit --date "$DOPT" --author "$AOPT" -m "$MSG"
        else
            git commit --date "$DOPT" -m "$MSG"
        fi
    else
        if [ -n "$AOPT" ]
        then
            git commit --author "$AOPT" -m "$MSG"
        else
            git commit  -m "$MSG"
        fi
    fi
    echo
}

# ----------------------------------------------------------------------
# Now let's get working on the repo history...
# Create the local repository
initialize $PROJECT $BASE

# Now let's get working on the repo history...
cd $BASE/$PROJECT

cat > quotes.txt <<EOF
This is a small collection of quotes only I would find humorous.
Good luck guessing where each quote comes from.

EOF

git add quotes.txt
git commit --date "-$DAY days" -m "Initial commit."
git remote add origin ../.remote-repos/da-quotes/
git push origin master
git branch --set-upstream-to=origin/master master

# Add each quote ...

addquote "Princess Bride" "Never go against a Sicilian when death is on the line!"
addquote "Princess Bride" "Murdered by pirates is good."
addquote "Holy Grail" "Now stand aside, worthy adversary." "'Tis but a scratch." "A scratch? Your arm's off."
addquote "Holy Grail" "I've had worse."
addquote "Holy Grail" "What... is the capital of Assyria?"
addquote "Holy Grail" "I don't want to talk to you no more, you empty-headed animal food trough wiper! I fart in your general direction! Your mother was a hamster and your father smelt of elderberries!"
addquote "Flying Circus" "It's certainly uncontaminated by cheese."

git push

# The next two quotes must be from the user, and should be "good" and "better"
# so that when we squash them, there is an obvious order to the commits...
echo "I will not have my fwends widiculed by the common soldiewy." >> quotes.txt
git commit -a --date "1 day" -m "Good quote from Life of Brian"
echo "Always look on the bright side of life." >> quotes.txt
git commit -a --date "1 day" -m "Better quote from Life of Brian"

echo "Now, you listen here! He's not the Messiah. He's a very naughty boy!" >> quotes.txt
git stash save "My favorite!"

# ----------------------------------------------------------------------
# To create merges to resolve, we create a new clone repository:

git clone $BASE/.remote-repos/$PROJECT $BASE/$OTHER
cd $BASE/$OTHER

# Now let's get working on the repo history...
cat > readme.txt <<EOF
This project contains quotes from various sources. I haven't quite
decided on the format for such work, but this is mostly to entertain
myself, and remind me of favorite books, movies and shows.

EOF

git add readme.txt
git commit -a --author "Ford Prefect <ford@prefect.org>" --date "-2 days" -m "Added a readme explaining the project"

echo "While I don't own the copyright to any of the quotes, the collection itself is in the public domain." >> readme.txt
git commit -a --author "Zaphod Beeblebrox <zod@hg2g.com>" --date "-1 day" -m "Added a license to the end of the readme."

git push origin master

echo
echo "All done. Checkout the goodies:   $BASE/$PROJECT"
