## Install

### One-line installer

If you want a one-line installer, this should do the trick:

      $ cd ~; wget https://github.com/trogdoro/xiki/archive/master.tar.gz; tar xzf master.tar.gz; xiki-master/bin/xsh

It will walk you through a couple setup steps.

### Manual install

You can download Xiki from: https://github.com/trogdoro/xiki/archive/master.tar.gz. Extract it into your home dir, or some other dir where you keep projects or downloaded things.

Then, to run (and optionally install xsh) just execute the 'xsh' command, located in the 'bin' dir:

      $ bin/xsh

### Git Install

Or, if you have git, you can get Xiki from github via "git clone https://github.com/trogdoro/xiki.git".  Then run "bin/xsh" from inside the dir.

### Troubleshooting

If you run into trouble, try running "./bin/xsh -i", which may give better error messages. Also, not that there's a cached "xsh forker" process that stays alive to speed up execution. Try "ps aux|grep forker" to find it. During trouble-shooting you may need to kill it.

### Supported platforms

Supported platforms: MacOS and Linux.  Pair with me if you want to see Windows support, support for your text editor, or just to hack on Xiki! (find me at twitter.com/xiki).

## Getting help

Try typing "xsh --examples", "xsh --help", or "xsh -help" on the command line.  Or, get help from a human:

Jump into the xiki chat room to chat about Xiki! I usually try to be in there during the day to answer questions. Use this link to jump right in, or use your own irc client to join.:

* http://webchat.freenode.net/?channels=xiki

Join the google group for help with installing:

* http://groups.google.com/group/xiki

Or tweet to @xiki:

* http://twitter.com/xiki

## Things to try

* Try from your normal shell
  * Type a command in your normal shell, and then Ctrl+X to filter down the output, instead of piping to grep.
  * Type Ctrl+R to explore you bash history in a more dynamic way.
  * Type "xsh ~" in your normal shell (or ~ and then Ctrl+X) to explore your home directory.

* Make a text file with some shell commands you often run (start them with "$ "). Then open the file in xsh ("xsh -o foo.txt") and use Ctrl+G to grab the commands and run them back in your shell, one at a time.
