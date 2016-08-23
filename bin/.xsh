#!/usr/bin/env bash

#
# This file should be included from your bash or zsh config. It
# defines key shortcuts (by default Ctrl+X and Ctrl+O) that let
# you quickly go back and forth between xsh and your existing
# shell. And it defines the 'xsh' command wrapper, which let's xsh
# send commands back to your shell.
#
# Example:
#
#   ~/.bashrc
#     : xiki_open_key="\C-o"        # Ctrl+X to expand in xsh
#     : xiki_key="\C-x"             # Ctrl+T to show a topic
#     : xiki_search_key="\C-s"      # Ctrl+S to search shared commands on XikiHub
#     : xiki_tasks_key="\C-t"     # Ctrl+O to show the options dropdown
#     : xiki_go_key="\C-g"          # Ctrl+G to grab commands between xsh and your shell
#     : xiki_reverse_key="\er"      # Ctrl+R to search shell history
#     : source ~/xiki_master/.xsh   # Enable the key shortcuts
#

# Wraps the 'xsh' shell command.  Calls it and then runs any commands
# it wrote to ~/.xiki/misc/tmp/grabbed_commands.xiki.
function xsh {

  # Always write history to a temp file, in case it was Ctrl+R, or they'll do a open+recent

  dir="$HOME/.xiki/misc/logs"
  mkdir -p $dir

  if [ -n "$ZSH_VERSION" ]; then
    fc -W "$dir/shell_external_log.xiki"
  else   # Assume bash or bash-compatible
    history -w "$dir/shell_external_log.xiki"
  fi

  # Run the actual xsh command...

  command xsh "$@"

  # Save xsh internal shell commands into our history...

  file="$HOME/.xiki/misc/tmp/recent_history_internal.xiki"
  if [ -f $file ]; then

    if [ -n "$ZSH_VERSION" ]; then
      fc -R $file
    else   # Assume bash or bash-compatible
      history -r $file
    fi
    # Be sure to delete it (-f in case user has weird settings for rm)
    rm -f $file
  fi


  # Grab any "go to shell" commands, and run it...

  if [ -f "$HOME/.xiki/misc/tmp/go_to_shell_commands.xiki" ]; then

    dir=`pwd`
    if [[ ! $dir =~ ^[a-zA-Z/_.-]+$ ]]; then
      dir="\"$dir\""
    fi
    cd_to_ignore="cd $dir"

    commandz=""

    # Run each line in the file
    while read -r p; do

      # Don't do redundant cd (to the same dir)
      if [ "$p" != "$cd_to_ignore" ]; then

        # Store in a var and eval later, since eval inside this
        # loop doesn't allow async commands like top, etc.
        commandz+="$p"
        commandz+=$'\n'

      fi

    done <$HOME/.xiki/misc/tmp/go_to_shell_commands.xiki

    # Delete it when we're done (-f in case user has weird settings for rm)
    rm -f $HOME/.xiki/misc/tmp/go_to_shell_commands.xiki

    SAVEIFS=$IFS   # Store original separator
    IFS=$'\n' y=($commandz)

    for i in "${y[@]}"; do

      i="${i%"${i##*[![:space:]]}"}"   # remove trailing whitespace characters

      # Add the command to the history...

      if [ -n "$ZSH_VERSION" ]; then
        this_command="xsh "$*
        if [[ ! $this_command = "xsh -r" ]]; then
          print -s $this_command
        fi
        print -s $i
      else   # Assume bash or bash-compatible
        this_command="xsh "$*
        if [[ ! $this_command = "xsh -r" ]]; then
          history -s $this_command
        fi
        history -s $i
      fi

      # Show the command...


      if [[ $i =~ ^\# ]]; then
        echo $i
      else
        echo \$ $i
      fi

      # Run the command, it's output will be shown...

      eval $i

    done

    IFS=$SAVEIFS   # Restore original separator

  fi

}

#
# Define key shortcuts...
#

# 'xiki open' shortcut (Ctrl+X) var exists, so define key...

if [ $xiki_open_key ]; then

  # If mac, run line un-blocking C-o
  if [ `uname` = 'Darwin' ]; then
    stty discard undef   # Let C-o be mapped
  fi

  # Make vim mode have C-a, since the below keys depend on it

  if [ -n "$ZSH_VERSION" ]; then
    bindkey -M viins '^a' beginning-of-line
  else   # Assume bash or bash-compatible
    bind -m vi-insert "\C-a":beginning-of-line
  fi

  # Define it...

  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_open_key '\C-axsh \n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_open_key'" "\C-axsh \n"'
  fi
fi

# 'xiki tasks' shortcut (Ctrl+T) var exists, so define key...

# "$ xsh -foo" version
if [ $xiki_key ]; then
  if [ $xiki_key = "\C-x" ]; then

    # It's Ctrl+X in bash, so undefine it first

    if [ $BASH_VERSION ]; then
      bind "'\C-x' end-of-line"   # Causes C-x to be bindable
    fi

    # It's Ctrl+X in zsh, so unset all the other ^X keys, so they don't cause a pause when typing ^X

    if [ $ZSH_VERSION ]; then
      bindkey -rp '\C-x'
    fi
  fi

  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_key '\C-axsh -\n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_key'" "\C-axsh -\n"'
  fi
fi

# 'xiki shared' shortcut (Ctrl+S) var exists, so define key...

if [ $xiki_search_key ]; then
  stty -ixon   # Let C-s be mapped

  if [ -n "$ZSH_VERSION" ]; then
    # bindkey -s $xiki_search_key '\C-axsh -s \n'
    bindkey -s $xiki_search_key '\C-axsh :\n'
  else   # Assume bash or bash-compatible
    # bind \"$xiki_search_key'" "\C-axsh -s \n"'
    bind \"$xiki_search_key'" "\C-axsh :\n"'
  fi
fi

# 'xiki grab menu' shortcut (Ctrl+G) var exists, so define key...

if [ $xiki_go_key ]; then
  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_go_key '\C-axsh =\n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_go_key'" "\C-axsh =\n"'
  fi
fi

# 'xiki open' shortcut (Ctrl+O) var exists, so define key...

if [ $xiki_tasks_key ]; then

  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_tasks_key '\C-axsh -t \n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_tasks_key'" "\C-axsh -t \n"'
  fi
fi


# 'xiki reverse' shortcut (Option+Ctrl+R) var exists, so define key...

if [ $xiki_reverse_key ]; then
  if [ -n "$ZSH_VERSION" ]; then
    bindkey -s $xiki_reverse_key '\C-axsh -r \n'
  else   # Assume bash or bash-compatible
    bind \"$xiki_reverse_key'" "\C-axsh -r \n"'
  fi
fi
