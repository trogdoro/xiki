## Xiki expands your command line

Xiki makes the command line friendlier and more powerful. Xiki Shell (xsh) lets you use Xiki from command line, in a way that augments your current favorite shell (bash or zsh).

    $ xsh

## Install

### One-line installer

If you want a one-line installer, this should do the trick. Copy and paste it into your shell console.

     curl -L https://raw.githubusercontent.com/trogdoro/xiki/actions/install_xsh -o ~/install_xsh; bash ~/install_xsh

It will walk you through a couple setup steps.

### Manual install

You can download Xiki from: https://github.com/trogdoro/xiki/archive/master.tar.gz. Extract it into your home dir, or some other dir where you keep projects or downloaded things.

Then, to run (and optionally install xsh) just execute the 'xsh' command, located in the 'bin' dir:

    $ ./install

### Git Install

Or, if you have git, you can get Xiki from github via "git clone https://github.com/trogdoro/xiki.git".  Then run "./install" from inside the dir.

## Tutorial

Try typing "xsh --tutorial", or "xsh --help" on the command line.  Or, get help from a human:

## Getting in touch

Tweet to @xiki:

* http://twitter.com/xiki

Join the google group for help with installing:

* http://groups.google.com/group/xiki

Or, jump into the xiki chat room to chat about Xiki! Use this link to jump right in, or use your own irc client to join:

* http://webchat.freenode.net/?channels=xiki

## Troubleshooting

If you run into trouble, try running "bin/xsh --d", which may give better error messages. Also, not that there's a cached "xsh forker" process that stays alive to speed up execution. Try "ps aux|grep forker" to find it. During trouble-shooting you may need to kill it.

## Supported platforms

Supported platforms: MacOS and Linux.  Pair with me if you want to see Windows support, support for your text editor, or just to hack on Xiki! (find me at twitter.com/xiki).

