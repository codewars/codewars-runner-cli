#!/bin/bash
set -e

# if CMD was empty or `listen`, do `tail -f /dev/null` to keep the container running
if [ "$#" -eq 0 -o "$1" = 'listen' ]; then
  set -- tail -f /dev/null
  if [ -r /runner/prewarm.sh ]; then
    bash /runner/prewarm.sh
  fi
else
  # otherwise, prepend node for `run-json`/`run`
  if [ "$1" = 'run-json' -o "$1" = 'run' ]; then
    set -- node "$@"
  fi
fi

exec "$@"
