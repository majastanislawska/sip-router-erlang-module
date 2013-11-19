#!/bin/sh

echo Starting with development console..

exec erl -pa $PWD/ebin \
         -pa $PWD/apps/*/ebin \
         -pa $PWD/deps/*/ebin \
         -pa $PWD/dev/*/ebin \
         -args_file `dirname $0`/start.args

