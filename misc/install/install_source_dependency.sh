# Called by xsh when starting up and you don't have dependencies installed (and if source code install is the only viable option for you)

set -e

function panic(){
  proj="`shift`"
  echo "`shift`"
  if [ -n "$proj" ]; then
    echo please look in /tmp/"$proj"_build.log for more details
  fi
  exit 1
}

mkdir -p /usr/local

if [ $1 = 'ruby' ]; then

  url="http://cache.ruby-lang.org/pub/ruby/2.0/ruby-2.0.0-p598.tar.gz"
  tarball="`basename $url`"
  extracted_dir="`basename -s '.tar.gz' $url`"
  cd /tmp
  wget --no-clobber $url                               || panic "" "wget failed"
  gunzip -c $tarball | tar xf -           || panic "" "untar failed"
  cd $extracted_dir
  echo putting ruby build logs in /tmp/ruby_build.log
  echo "" > ruby_build.log
  chmod 777 ruby_build.log
  ./configure --prefix=/usr/local/  >/tmp/ruby_build.log  || panic "ruby" "configure failed"
  make                             >>/tmp/ruby_build.log  || panic "ruby" "make failed"
  make test                        >>/tmp/ruby_build.log  || panic "ruby" "make test failed"
  make install                     >>/tmp/ruby_build.log  || panic "ruby" "make install failed"
  cd ..
  rm -rf $tarball $extracted_dir

else

  url="http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.gz"
  tarball="`basename $url`"
  extracted_dir="`basename -s '.tar.gz' $url`"
  cd /tmp
  wget --no-clobber $url                               || panic "" "wget failed"
  gunzip -c $tarball | tar xf -           || panic "" "untar failed"
  cd $extracted_dir
  echo putting emacs build logs in /tmp/emacs_build.log
  echo "" > emacs_build.log
  chmod 777 emacs_build.log
  ./configure --prefix=/usr/local/  >/tmp/emacs_build.log  || panic "emacs" "configure failed"
  make                             >>/tmp/emacs_build.log  || panic "emacs" "make failed"
  make install                     >>/tmp/emacs_build.log  || panic "emacs" "make install failed"
  cd ..
  rm -rf $tarball $extracted_dir

fi
