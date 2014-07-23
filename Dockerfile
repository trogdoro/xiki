FROM ubuntu:14.04

RUN apt-get update
RUN apt-get -y install make
RUN apt-get -y install git
RUN apt-get -y install ruby1.9.3
RUN gem1.9.3 install bundler
ADD ./ xiki
RUN cd xiki ; bundle
RUN cd xiki ; ruby1.9.3 etc/command/copy_xiki_command_to.rb /usr/bin/xiki
