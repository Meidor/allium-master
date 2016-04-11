#
# Erlang build environment.
#
# docker build -t erlang_rebar3 .
# APP_SOURCE_DIR is the folder containing the rebar.config file. 
# docker run --rm --name erlang_rebar3 -v  $APP_SOURCE_DIR:/erlang_app erlang_rebar3
#

# reuse the image already created 
FROM erlang

MAINTAINER Rik Harink <rikharink@gmail.com>

RUN apt-get update
RUN apt-get -y install git

RUN mkdir /build
WORKDIR /build/

ADD build_erlang.sh /build/
ADD https://s3.amazonaws.com/rebar3/rebar3 /build/
RUN chmod a+x /build/rebar3
RUN chmod a+x /build/build_erlang.sh

VOLUME /erlang_app

CMD ["nouser"]
ENTRYPOINT ["/build/build_erlang.sh","/erlang_app","/build"]