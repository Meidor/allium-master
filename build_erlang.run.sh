#!/bin/sh
# run build and application

#########################################
#      START BUILD                      #
#########################################

/usr/bin/redis-server --daemonize yes
/usr/bin/redis-cli ping

#rebar3 clean
#rebar3 deps
#rebar3 compile
rebar3 release
cd "_build/default/rel/master/bin"
nohup ./master console
