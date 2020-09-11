#!/bin/bash

find $(dirname $0) -maxdepth 1 -name '.[!.]*' -not -name '.git' -exec ln -isv {} $HOME \;
echo "finished."
