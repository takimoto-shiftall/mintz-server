FROM resin/raspberry-pi2-debian:stretch
RUN mkdir -p /var/log
RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get install -y curl
RUN apt-get install -y wget
RUN apt-get install -y libpq-dev
RUN apt-get install -y llvm-3.9
RUN apt-get install -y libtinfo-dev
RUN apt-get install -y libgmp-dev
RUN apt-get install -y zlib1g-dev
RUN wget https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-arm.tar.gz
RUN tar xzvf stack-1.7.1-linux-arm.tar.gz
RUN mkdir -p /root/.local/bin
RUN cp stack-1.7.1-linux-arm/stack /root/.local/bin/
RUN /root/.local/bin/stack update
# RUN apt-get install -y haskell-stack
# RUN curl -sSL https://get.haskellstack.org/ | sh
EXPOSE 8001

# ENTRYPOINT ["/bin/sh", "-c", "while true; do sleep 10; done"]