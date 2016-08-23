FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get install -y emacs

# Ruby
RUN apt-get install -y ruby

# Git
RUN apt-get install -y git

COPY . /xiki/
RUN /xiki/bin/xsh --install

WORKDIR /root

CMD ["bash", "/xiki/bin/xsh"]

