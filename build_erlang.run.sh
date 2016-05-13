#!/bin/sh
# run build and application

#########################################
#      START BUILD                      #
#########################################

/usr/bin/redis-server --daemonize yes

cd "_build/default/rel/master/bin"
nohup ./master console
