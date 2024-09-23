FROM debian

RUN apt-get update && apt-get install -y git curl linux-perf build-essential libssl-dev libncurses5-dev && \
    curl -L https://raw.githubusercontent.com/kerl/kerl/master/kerl > /usr/bin/kerl && \
    chmod +x /usr/bin/kerl && \
    kerl update releases

ENV CFLAGS="-march=skylake -O2 -g"
ENV MAKEFLAGS="-j8"
ENV ERLC_USE_SERVER=true

RUN git clone https://github.com/garazdawi/otp -b john/erts/ets-mvcc-experiments-27.1 otp

RUN cd otp && ./otp_build all -a && cd release/*/ && ./Install -minimal `pwd`