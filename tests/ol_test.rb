require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require 'ol'

class OlTest < Test::Unit::TestCase

  def test_parse_line
    line = "/tmp/foo/bar.rb:314:in `l_keys'"
    assert_equal(
      {:path=>'/tmp/foo/bar.rb', :line=>'314', :method=>'l_keys', :clazz=>'Bar'},
      Ol.parse_line(line))
  end

end
