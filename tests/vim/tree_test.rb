require File.expand_path('../helpers.rb', __FILE__)
require 'xiki/vim'

class String
  def n
    self.gsub(/^\s*/, '').strip
  end
end

module XikiVim
  class TestTree < MiniTest::Unit::TestCase
    def test_parse
      docs_faq = <<eodocs
docs/faq
  + What is Xiki/
  + What does Xiki stand for/
  + What are Xiki's main dependencies/
  + What's the best way to figure out how to do something/
  + How can I keep up to date with the latest Xiki happenings/
eodocs

      what_xiki = <<eoxiki
Xiki is an environment that can be used in several ways:

- Shell terminal on steroids
- Development environment for coding rails or node.js apps, etc.
- Framework for making lightweight user interfaces
eoxiki

      empty = Tree.new ""
      assert_equal 0, empty.children.length

      iptree = Tree.new "192.168.0.14"
      assert_equal 1, iptree.children.length

      docstree = Tree.new docs_faq
      assert_equal 1, docstree.children.length
      assert_equal 5, docstree.children[0].children.length

      buffer = FakeBuffer.new []
      docstree.render(buffer)
      assert_equal docs_faq.n, buffer.dump.n

      buffer = FakeBuffer.new []
      xikitree = Tree.new what_xiki
      xikitree.render(buffer)
      assert_equal what_xiki.n, buffer.dump.n
    end
  end
end

# vim: nowrap
