require 'rubygems'
require 'test/unit'
$:.unshift "../"
require 'remote'

class RemoteTest < Test::Unit::TestCase

  def test_split_root
    assert_equal(['user', 'foo.com', '2122', '/var/main'], Remote.split_root('user@foo.com:2122/var/main'))
    assert_equal(['user', 'foo.com', '2122', '/'], Remote.split_root('user@foo.com:2122/'))
    assert_equal(['user', 'foo.com', '2122', '/'], Remote.split_root('user@foo.com:2122'))
  end

  def test_sort
    txt = "ajax/
        apcx.orig
        css/
        f.wav
        ".gsub(/^ +/, '')

    expected = ["ajax/", "css/", "apcx.orig", "f.wav"]
    assert_equal(expected, Remote.sort(txt))
  end

  def test_split_root
    assert_equal(["xiki", "xiki.org", "222", "/tmp"], Remote.split_root("xiki@xiki.org:222/tmp/"))
    assert_equal(["xiki", "xiki.org", "22", "/tmp"], Remote.split_root("xiki@xiki.org/tmp/"))
    assert_equal(["xiki", "xiki.org", "22", "/tmp"], Remote.split_root("/xiki@xiki.org/tmp/"))
  end

end
