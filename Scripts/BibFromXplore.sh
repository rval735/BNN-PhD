#!/bin/bash

echo "About to retrieve Bibitem from IEEEXplore with ID:" $1

# Full URL example: https://ieeexplore-ieee-org.ezproxy.auckland.ac.nz/document/7838429

if [[ ! $1 =~ [0-9]{7} ]]; then
  echo "This script needs a record ID from IEEEXplore like: 7838429 (7 numbers)"
  exit 0
fi

FULLREF="https://ieeexplore.ieee.org/xpl/downloadCitations?download-format=download-bibtex&citations-format=citation-abstract&recordIds="$1

#FULLREF="https://ieeexplore.ieee.org/xpl/downloadCitations?download-format=download-bibtex&citations-format=citation-abstract&recordIds=7838429"

HTMLBIB=$(curl -s $FULLREF)
CLEANED=$(echo $HTMLBIB | perl -pe 's/<br>//g')
NAME=$(echo -e $CLEANED | perl -ne 'print $1 if /\stitle\=\{([\w*|\W*|\s*|\d*|\D*]*)},\s*y/s')
NAME2=$(echo -e $NAME | perl -pe 's/[:|\/]/-/g')

if [ -z "$NAME2" ]
then
    echo "File saved in $1.bib"
    echo $CLEANED >> "$1.bib"
else
    echo "File saved in $NAME2.bib"
    echo $CLEANED >> "$NAME2.bib"
fi
