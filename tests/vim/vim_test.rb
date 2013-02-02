require File.expand_path('../helpers.rb', __FILE__)

class TestVim < MiniTest::Unit::TestCase
  def test_construct_path
    ary = ['docs/faq', '  + What is Xiki/', '  + What does Xiki stand for/']
    buffer = FakeBuffer.new ary
    buffer.line_number = 1
    assert_equal "docs/faq/What is Xiki/", XikiVim.construct_path(buffer)

    ary = ['docs/faq']
    buffer = FakeBuffer.new ary
    assert_equal "docs/faq/", XikiVim::construct_path(buffer)

    ary = ['ls      ']
    buffer = FakeBuffer.new ary
    assert_equal "ls/", XikiVim::construct_path(buffer)
  end

  def test_ensure_format
    buffer = FakeBuffer.new ["docs"]
    XikiVim::ensure_format buffer
    assert_equal "+ docs", buffer.dump
  end
end

# vim: nowrap
