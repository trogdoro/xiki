# Summary
This file tells how to install Xiki.  See http://xiki.org for a description of Xiki.

Either install as a gem, or install from github.

# Install Xiki

## As a gem

      $ gem install xiki

## From github

      $ git clone git@github.com:trogdoro/xiki.git
      $ cd xiki
      $ bundle install --system
      $ cp <xiki dir>/etc/command/xiki_wrapper /usr/local/bin/xiki
      $ chmod 755 /usr/local/bin/xiki

# Verify the 'xiki' command works

      $ xiki

# Configure Emacs to use Xiki

If you're going to use Xiki with Emacs, do these steps:

## Step 1: Download Emacs

### On Mac:
- Try Aquamacs 1.9.1 if you're new to emacs (it's more mac-like)
  - http://aquamacs.org/download-classic.shtml
- Or
  - http://emacsformacosx.com/emacs-builds/Emacs-22.3-i386-10.5.7.dmg
- Note: newer mac versions (after 22) have a bug that makes communicating with processes 1000 times slower.  Til this is fixed stick with the above versions.

### On Linux:

      $ sudo apt-get install emacs


## Step 2: Do extra EmacsRuby (el4r) steps

      $ gem contents el4r | grep setup.rb   # To see which dir to cd to in the next step
      $ cd /Library/Ruby/Gems/1.8/gems/el4r-1.0.4/
      $ ruby setup.rb
      $ cd bin/
      $ ruby -S el4r-rctool -p
      $ sudo ruby -S el4r-rctool -i

## Step 3: Require Xiki in EmacsRuby's config
Sample configuration:

      ~/.el4r/init.rb:
        $LOAD_PATH.unshift "/projects/xiki/lib"
        require 'xiki'
        Xiki.init

        KeyBindings.keys   # Use default key bindings
        Styles.use_xiki_color_scheme   # Use xiki's color scheme


## Trouble-shooting
The install is a bit rough at the moment...

- If you get an error (and you probably will)
  - If you got partially through the load
    - you will be able to use these keys to trouble-shoot:
      - Option-l to reload xiki and .emacs
        - also use this when you see "el4r-instance is dead"
      - Option-e to look at the latest error
    - It will be Command instead of Option if you're not using Aquamacs

  - Or you can manually restart emacs (or reload .emacs) and look at the log
    - Named something like: /tmp/el4r......log
  - Go to the end and search backward for the last error
    - probably contains ":Error:"

- If you get a "can't find header dir for ruby" error
  $ sudo apt-get install ruby1.8-dev

- If you run into trouble installing EmacsRuby
  - See: http://www.rubyist.net/~rubikitch/computer/el4r/index.en.html
    - Click on 'Download / Install / Setup' link


## Google group
Join the google group for help with installing, or to chat about whatever or
share your ideas:

  http://groups.google.com/group/xiki

