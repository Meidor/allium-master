#!/bin/sh
# run build and application

#########################################
#      START BUILD                      #
#########################################

rebar3 clean
rebar3 deps
rebar3 compile
rebar3 release
cd "_build/default/rel/master/bin"
nohup ./master console
