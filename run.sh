#! /bin/sh
BASEDIR=`dirname $0`
BASEDIR=`readlink -f $BASEDIR`
cd $BASEDIR

ERL_LIBS=$BASEDIR:$BASEDIR/deps
export ERL_LIBS

exec erl +K true -noinput -noshell \
        -sasl errlog_type error \
        -sname ircbot@localhost \
        -s ircbot_app -conf settings.cfg
