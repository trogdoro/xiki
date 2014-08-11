#!/usr/bin/env ruby

print "Content-type: text/html\n\n";

require "../../lib/xiki/ol.rb"
require "web_server"
require "cgi"
WebServer.index
