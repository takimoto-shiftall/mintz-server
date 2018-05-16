FROM resin/raspberry-pi2-debian:stretch
RUN mkdir -p /var/log
RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y libpq-dev
RUN apt-get install -y llvm-3.9
RUN apt-get install -y libtinfo-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
EXPOSE 8001

ENTRYPOINT ["/bin/sh", "-c", "while true; do sleep 10; done"]