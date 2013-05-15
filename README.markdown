## Summary

This file shows how to install Xiki.  Xiki is like a shell console with GUI features.  It has a web interface, but rens best from a Xiki-enabled text interface.  It has menus for quickly getting started with using various tools and libraries.  Also you can easily create your own menus.

See http://xiki.org

## Install Ruby 1.9.3

On ubuntu, do: $ sudo apt-get install ruby1.9.3.

On the mac, install rvm (http://google.com/search?q=install+rvm) and the do: $ rvm install ruby-1.9.3

## Install Xiki

Either install as a gem, or install from github.

### As a gem

      $ gem install xiki --pre

### Or, from github

      $ git clone git://github.com/trogdoro/xiki.git
      $ cd xiki
      $ git checkout v1.0     # Only required temporarily, until the branch is merged in
      $ sudo gem install bundler   # <- no "sudo" if using rvm
      $ sudo bundle                # <- no "sudo" if using rvm
      $ sudo ruby etc/command/copy_xiki_command_to.rb /usr/bin/xiki   # or /usr/local/bin/xiki and no "sudo" if using rvm

## Verify the 'xiki' shell command works

      $ xiki

It should delay slightly the first time, but be fast subsequent
times.  If you run into errors and then fix them, you'll want to
run the "xiki restart" command.

## Bring up the Xiki web interface

      $ xiki web/start

Then follow the instructions you see (it tells you to go to http://localhost:8161).

## Getting help

Join the google group for help with installing, or to chat about
whatever or share your ideas:

http://groups.google.com/group/xiki

Or tweet to @xiki.
