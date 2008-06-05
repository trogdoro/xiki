require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require '../auto_menu'

class AutoMenuTest < Test::Unit::TestCase

  def test_child_bullets
    tree =
      "- .approvals(3)
       - .reminders
       - .backup/
         - .backup_code()/
         - .backup_pages()
         hey
       - .run_remote_server
       ".gsub(/^       /, '')

    # Get items under .backup
    assert_equal(
      "- .backup_code()/\n- .backup_pages()\nhey\n",
      AutoMenu.child_bullets(tree, "backup"))

  end
end
