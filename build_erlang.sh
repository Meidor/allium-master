#!/bin/bash -ev
#
# run build and test (unit and integration)
#
set -e -v

ERLANG_APP=$1
BUILD_DIR=$2
SSH_USER=$3

PATH="$PATH:/build"

#########################################
#      START BUILD                      #
#########################################
if [ -d "${BUILD_DIR}/tmp" ]
 then
  rm -rf "${BUILD_DIR}/tmp"
 fi

cp -a "${ERLANG_APP}" "${BUILD_DIR}/tmp"
cd "${BUILD_DIR}/tmp"

rebar3 deps
rebar3 as prod compile
rebar3 as prod release tar
rebar3 ct
# pack up the test results
TIMESTAMP=`date "+%Y%m%d_%H%M"`
tar cf "/erlang_app/ct_logs_${TIMESTAMP}.tgz" -C "${BUILD_DIR}/tmp/_build/test" logs

# Copy jenkins code coverage log
cp "${BUILD_DIR}/tmp/_build/test/logs/jenkins_report.xml" "/erlang_app/jenkins_report.xml"