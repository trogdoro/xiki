FROM ubuntu:14.04

RUN apt-get update
RUN apt-get install -y curl
WORKDIR /root
RUN curl -LO https://github.com/trogdoro/xiki/archive/master.tar.gz
RUN tar xzf master.tar.gz

RUN echo "Run these commands:" >> README
RUN echo "$ cd xiki-master/bin" >> README
RUN echo "$ ./xsh" >> README

