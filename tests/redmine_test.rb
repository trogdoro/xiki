require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require '../redmine'
require '../core_ext'

class RedmineTest < Test::Unit::TestCase

  # Should correctly format simple diff
  def test_redmine_to_xiki
    before =
      "#Dev Team Todo
       ##h2
       * Ad
       * 70
       ** Make them show
       ".unindent

    after =
      "| Dev Team Todo
       || h2
       - Ad
       - 70
         - Make them show
       ".unindent

    assert_equal(after, Redmine.redmine_to_xiki(before))
  end

  # Should correctly format simple diff
  def test_xiki_to_redmine
    before =
      "| Dev Team Todo
       || h2
       - Ad
       - 70
         - Make them show
       ".unindent

    after =
      "#Dev Team Todo
       ##h2
       * Ad
       * 70
       ** Make them show
       ".unindent

    assert_equal(after, Redmine.xiki_to_redmine(before))
  end

end
