## Summary

This file shows how to install Xiki.  Xiki is like a shell console with GUI features.  It is also a glue language.  It ties together many of the things we developers use.  Such as shell commands, code, files, directories, notes, urls, database queries, and many other things.  It accomplishes this by using a wiki-ish text syntax that can be expanded directly from a text editor.  See http://xiki.org/screencasts if you have no idea what I'm talking about. :)

It has a web interface, but runs best from a Xiki-enabled text editor.  It has menus for quickly getting started with using various tools and libraries.  Also you can easily create your own menus, in about ten different ways.  Many of them will seem obvious to you.

## Install Ruby 1.9.3

On ubuntu, do: $ sudo apt-get install ruby1.9.3.

On the mac, install rvm (http://google.com/search?q=install+rvm) and the do: $ rvm install ruby-1.9.3

## Install Xiki

Either install as a gem, or install from github.

### Install from github

      $ git clone git://github.com/trogdoro/xiki.git
      $ cd xiki
      $ sudo gem install bundler   # <- no "sudo" if using rvm
      $ sudo bundle                # <- no "sudo" if using rvm
      $ sudo ruby etc/command/copy_xiki_command_to.rb /usr/bin/xiki   # or /usr/local/bin/xiki and no "sudo" if using rvm

### Or, as a gem

## Note: at the moment this will give you the old Xiki version.  Use the "github" instructions.

      $ gem install xiki --pre

## Verify the 'xiki' shell command works

      $ xiki

It should delay slightly the first time, but be fast subsequent
times.  If you run into errors and then fix them, you'll want to
run the "xiki stop" command.  See the process log for
trouble-shooting (/tmp/xiki_process.rb.output).

## Bring up the Xiki web interface

      $ xiki web/start

Then follow the instructions you see (it tells you to go to http://localhost:8161).

See the web log for trouble-shooting (/tmp/xiki_web.output).

## Getting help

Join the google group for help with installing, or to chat about
whatever or share your ideas:

http://groups.google.com/group/xiki

Or tweet to @xiki.
