#!/bin/sh
# run build and test (unit and integration)

#########################################
#      START BUILD                      #
#########################################
if [ -d "/build/tmp" ]
 then
  rm -rf "/build/tmp"
 fi

cp -a "/erlang_app" "/build/tmp"
cd "/build/tmp"

rebar3 deps
rebar3 as prod compile
rebar3 as prod release tar
rebar3 ct

# pack up the test results
TIMESTAMP=`date "+%Y%m%d_%H%M"`
tar cf "/erlang_app/ct_logs_${TIMESTAMP}.tgz" -C "/build/tmp/_build/test" logs

# Copy jenkins code coverage log
cp "/build/tmp/_build/test/logs/jenkins_report.xml" "/erlang_app/jenkins_report.xml"