# Summary

This file shows how to install Xiki.  For a description of Xiki see:

http://xiki.org

# Install Xiki

Either install as a gem, or install from github.

## As a gem

      $ sudo gem install xiki

## Or, from github

      $ git clone git@github.com:trogdoro/xiki.git
      $ cd xiki
      $ sudo gem install bundler
      $ sudo bundle install --system
      $ sudo ruby etc/command/copy_xiki_command_to.rb /usr/local/bin/xiki

You don't need "sudo" at the beginning if you're using rvm and your
/usr/local/bin/ dir is writable.

# Verify the 'xiki' shell command works

      $ xiki

It should delay slightly the first time, but be fast subsequent
times.  If you run into errors and then fix them, you'll want to
run the "xiki restart" command.

# Configure your editor to use Xiki

## Emacs

Emacs is the most supported editor.  Don't worry though, you don't
have to know emacs to use it with Xiki.

### Step 1: Download Emacs

#### On Mac:

- Try Aquamacs if you're new to emacs (it's more mac-like)
   - http://aquamacs.org/download-classic.shtml
   - works well with rvm, because it loads .bash_login
- Or
   - http://emacsformacosx.com

#### On FreeBSD:
     $ pkg add emacs
     or
     $ make -C /usr/ports/editors/emacs install clean

#### On Linux:

      $ sudo apt-get install emacs
      or
      $ sudo yum install emacs

#### On Windows:

We just patched el4r, so there's a chance Xiki might work in windows.

- Skip the 'xiki' shell command step
- Maybe try this emacs? http://ourcomments.org/Emacs/EmacsW32.html

### Step 2: EmacsRuby (el4r) setup

      $ cd `xiki directory`
      $ sudo bash etc/install/el4r_setup.sh

If you're using rvm, the sudo may not be necessary.

### Step 3: Require Xiki in EmacsRuby's config
Sample configuration:

      ~/.el4r/init.rb:
        $LOAD_PATH.unshift "(xiki directory)/lib"
        require 'xiki'
        Xiki.init

        KeyBindings.keys   # Use default key bindings
        Themes.use "Default"  # Use xiki theme

Be sure to substitute '(xiki directory)' with the actual dir.  If you
don't know it, run this command:

      $ xiki directory

### If you get an error

If you got partially through the load...

- You may be able to use these keys to trouble-shoot:
   - Option+e to look at the latest error in the log
   - Option+l to reload xiki and .emacs
      - also use this when you see "el4r-instance is dead"
- If you can't use the keys, look at the log
   - Named something like: /tmp/el4r......log
   - Go to the end and search backward for the last error
      - probably contains ":Error:"
   - Restart emacs (or reload .emacs) manually to reload
- See "Issues Loading Xiki" buffer (under "Window" menu bar menu)

## Vim

Vim support is very partially implemented, but should be pretty
straight-forward to implement.  It turns out vim is very easy to
extend using ruby.  See this file to try it out:

(xiki directory)/etc/vim/vim_status.notes

## CodeMirror

There's a simple prototype working, though it's not committed yet.

## Other editors

Are you in the bay area and savvy at extending your editor (vim,
sublime, textmate, rubymine)?  Ping me on the google group and
we'll get together and pair on making a Xiki extension for it.

# Google group

Join the google group for help with installing, or to chat about
whatever or share your ideas:

http://groups.google.com/group/xiki

