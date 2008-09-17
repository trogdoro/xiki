require 'test/unit'
require 'el_mixin'
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

    expected = "ajax/
      css/
      apcx.orig
      f.wav
      ".gsub(/^ +/, '')
    assert_equal(expected, Remote.sort(txt))
  end
end
