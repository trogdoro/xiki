require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require '../code'

class CodeTest < Test::Unit::TestCase

  # Fails because we aren't actually running ruby
#   def test_eval
#     returned, stdout = Code.eval("puts 'hi'")
#     assert_equal(
#       "hi", stdout
#       )
#   end
end
