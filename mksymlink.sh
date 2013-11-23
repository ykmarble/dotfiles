#!/bin/sh

find $(pwd) -depth 1 -name '.[!.]*' -not -name '.git' -exec ln -isv {} $HOME \;
echo "finished."
