require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require 'text_util'

class TextUtilTest < Test::Unit::TestCase

  def test_space
    before =
"  hey
    you
"
    after =
"hey
  you
"
    assert_equal(after, TextUtil.unindent(before))
  end

  def test_with_inital_linebreak
    before =
"
  hey
    you
"
    after =
"hey
  you
"
    assert_equal(after, TextUtil.unindent(before))
  end


  def test_tabs
    before =
"\they
\tyou
"
    after =
"hey
you
"
    assert_equal(after, TextUtil.unindent(before))
  end

  def test_space_on_2nd_line
    before =
      "Dev Todo
       * Ad
       * 70:
       "
    after =
"Dev Todo
* Ad
* 70:
"
    assert_equal(after, TextUtil.unindent(before))


    # Shouldn't do if only 2 spaces over
    before =
      "Dev Todo
  * Ad
  * 70:
"
    after =
"Dev Todo
  * Ad
  * 70:
"
    assert_equal(after, TextUtil.unindent(before))

  end


  def test_snake_case
    assert_equal("core_platform", TextUtil.snake_case("CorePlatform"))
    assert_equal("/core_platform", TextUtil.snake_case("/CorePlatform"))
  end

end
