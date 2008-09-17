require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require 'diff_log'

class TreeLsTest < Test::Unit::TestCase

  # Should correctly format simple diff
  def test_format
    formatted = DiffLog.format("/projects/xiki/trunk/", "diff_log.rb",
      "|--- /projects/xiki/trunk/diff_log.rb	2008-03-03 20:35:30.000000000 -0500
       |+++ /tmp/latest-diff.txt	2008-03-03 20:35:44.000000000 -0500
       |@@ -56,0 +57 @@
       |+#new
       |".gsub(/^ *\|/, '')
       )

    result =
      "|/projects/xiki/trunk/
       |  diff_log.rb
       |    :57
       |      +|#new
       |".gsub(/^ *\|/, '')
    assert_equal(result, formatted)
  end

  # Should correctly format simple diff
  def test_format_without_path
    formatted = DiffLog.format("/projects/xiki/trunk/", "diff_log.rb",
      "|--- /projects/xiki/trunk/diff_log.rb	2008-03-03 20:35:30.000000000 -0500
       |+++ /tmp/latest-diff.txt	2008-03-03 20:35:44.000000000 -0500
       |@@ -56,0 +57 @@
       |+#new
       |".gsub(/^ *\|/, '')
       )

    result =
      "|/projects/xiki/trunk/
       |  diff_log.rb
       |    :57
       |      +|#new
       |".gsub(/^ *\|/, '')
    assert_equal(result, formatted)
  end
end
