# Called by xsh when starting up and you don't have dependencies installed (and if source code install is the only viable option for you)

set -e

function panic(){
  proj="`shift`"
  echo "`shift`"
  if [ -n "$proj" ]; then
    echo please look in ~/.xiki_dependencies/"$proj"_build.log for more details
  fi
  exit 1
}

if [ "$1" = 'ruby' ]; then

  url="http://cache.ruby-lang.org/pub/ruby/2.0/ruby-2.0.0-p598.tar.gz"

  tarball="`basename $url`"
  extracted_dir="`basename -s '.tar.gz' $url`"
  mkdir -p ~/.xiki_dependencies
  cd ~/.xiki_dependencies
  wget --no-clobber $url                               || panic "" "wget failed"

  echo "4136bf7d764cbcc1c7da2824ed2826c3550f2b62af673c79ddbf9049b12095fd  ruby-2.0.0-p598.tar.gz" \
    | sha256sum -c  || panic "" "sha256sum failed"

  gunzip -c $tarball | tar xf -           || panic "ruby" "untar failed"
  cd $extracted_dir
  echo putting ruby build logs in `pwd`/ruby_build.log
  touch ruby_build.log
  chmod 644 ruby_build.log
  ./configure --prefix=~/.xiki_dependencies  >/tmp/ruby_build.log  || panic "ruby" "configure failed"
  make                             >>/tmp/ruby_build.log  || panic "ruby" "make failed"
  make test                        >>/tmp/ruby_build.log  || panic "ruby" "make test failed"
  make install                     >>/tmp/ruby_build.log  || panic "ruby" "make install failed"

else

  url="https://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.gz"
  tarball="`basename $url`"
  extracted_dir="`basename -s '.tar.gz' $url`"
  mkdir -p ~/.xiki_dependencies
  cd ~/.xiki_dependencies
  wget --no-clobber $url                               || panic "" "wget failed"
  gunzip -c $tarball | tar xf -           || panic "" "untar failed"
  cd $extracted_dir
  echo putting emacs build logs in `pwd`/emacs_build.log
  touch emacs_build.log
  chmod 644 emacs_build.log
  ./configure --prefix=~/.xiki_dependencies  >/tmp/emacs_build.log  || panic "emacs" "configure failed"
  make                             >>/tmp/emacs_build.log  || panic "emacs" "make failed"
  make install                     >>/tmp/emacs_build.log  || panic "emacs" "make install failed"

fi
