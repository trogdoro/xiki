require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require 'ol'
require 'core_ext'
require 'yaml'
require 'keys'

class GitTest < Test::Unit::TestCase

  def test_words_to_letters
    assert_equal "TE", Keys.words_to_letters("to_end")
  end

  def test_status_to_hash
    assert_equal "C-t C-e", Keys.translate_keys("TE")
    assert_equal "M-t C-e", Keys.translate_keys("_TE")
  end

end
