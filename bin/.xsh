#!/bin/bash

#
# This file should be included from your bash or zsh config. It
# defines key shortcuts (by default Ctrl+X and Ctrl+G) that let
# you quickly go back and forth between xsh and your existing
# shell. And it defines the 'xsh' command wrapper, which let's xsh
# send commands back to your shell.
#
# Example:
#
#   ~/.bashrc
#     : xiki_open_key="\C-x"   # Ctrl+X to open xsh
#     : xiki_drop_key="\C-d"   # Ctrl+D to show dropdown
#     : xiki_grab_key="\C-g"   # Ctrl+G to grab to and from xsh
#     : source /Users/craig/Dropbox/xiki/bin/.xsh
#

# Wraps the 'xsh' shell command.  Calls it and then runs any commands
# it wrote to ~/xiki/misc/tmp/grabbed_commands.notes.
function xsh {

  # Ctrl+D on blank line, so do normal shell behavion and exit (zsh gives us control when Ctrl+D on a blank line)...

  if [ "$1" = "-d" ] && [ $# = 1 ]; then
    echo "exit"
    exit
  fi

  # Esc, Ctrl+R, so write history to a temp file

  if [ "$1" = "-r" ]; then
    dir="$HOME/xiki/misc/tmp"
    mkdir -p $dir
    history -w "$dir/reverse_history.notes"
  fi

  command xsh $*

  # Grab any "go" text, and run it...

  if [ -f "$HOME/xiki/misc/tmp/grabbed_commands.notes" ]; then

    dir=`pwd`
    if [[ ! $dir =~ ^[a-zA-Z/_.-]+$ ]]; then
      dir="\"$dir\""
    fi
    cd_to_ignore="cd $dir"

    commandz=""

    # Run each line in the file
    while read p; do

      # Don't do redundant cd (to the same dir)
      if [ "$p" != "$cd_to_ignore" ]; then

        # Store in a var and eval later, since eval inside this
        # loop doesn't allow async commands like top, etc.
        commandz+=$p
        commandz+=$'\n'

      fi

    done <$HOME/xiki/misc/tmp/grabbed_commands.notes

    # Delete it when we're done
    rm $HOME/xiki/misc/tmp/grabbed_commands.notes

    SAVEIFS=$IFS   # Store original separator
    IFS=$'\n' y=($commandz)

    for i in "${y[@]}"; do

      i="${i%"${i##*[![:space:]]}"}"   # remove trailing whitespace characters

      if [ -n "$ZSH_VERSION" ]; then
        print -s $i
      else   # Assume bash or bash-compatible
        history -s $i
        # It seems to be replacing :(
        # Todo > deal with this later
        # - maybe write the current command too?
        #   - how would we know that? > see what's in end of the history now?
        #   - see how it works in newer bash first
      fi

      # Show the command
      echo \$ $i
      # Run the command, it's output will be shown
      eval $i
    done

    IFS=$SAVEIFS   # Restore original separator

  fi

  # Todo > have key that echos the output of the command
  #   - to > the external shell
  #   - ideas > up+grab, as+output__, window+output__, jump+__
  # echo -e "~/\n  - a/\n  - b/"

}

#
# Define key shortcuts...
#

# 'xiki open' shortcut (usually Ctrl+X) var exists, so define key...

if [ $xiki_open_key ]; then

  # It's Ctrl+X in bash, so undefine it first...

  if [ $BASH_VERSION ] && [ $xiki_open_key = "\C-x" ]; then
    bind "'\C-x' end-of-line"   # Causes C-x to be bindable
  fi

  # Define it

  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_open_key '\C-axsh \n'
  else   # Assume bash or bash-compatible

    #echo "heyyy"
    # bind '"'$xiki_open_key'" "\C-axsh \n"'
    bind \"$xiki_open_key'" "\C-axsh \n"'

  fi
fi

# 'xiki grab' shortcut (usually Ctrl+G) var exists, so define key...

if [ $xiki_grab_key ]; then
  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_grab_key '\C-axsh -g \n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_grab_key'" "\C-axsh -g \n"'
  fi
fi

# 'xiki dropdown' shortcut (usually Ctrl+D) var exists, so define key...

if [ $xiki_dropdown_key ]; then
  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_dropdown_key '\C-axsh -d \n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_dropdown_key'" "\C-axsh -d \n"'
  fi
fi


# TODO > probably remove this > Drop+Tab is probably sufficient
# 'xiki tab' shortcut (usually Option+Tab) var exists, so define key...

if [ $xiki_tab_key ]; then
  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_tab_key '\C-axsh -t \n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_tab_key'" "\C-axsh -t \n"'
  fi
fi

# 'xiki reverse' shortcut (usually Option+Ctrl+R) var exists, so define key...

if [ $xiki_reverse_key ]; then
  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_reverse_key '\C-axsh -r \n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_reverse_key'" "\C-axsh -r \n"'
  fi
fi
