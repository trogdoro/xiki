require File.expand_path('../../test_helpers.rb', __FILE__)
require 'minitest/autorun'
require 'xiki/vim'

class FakeBuffer
  attr_accessor :line_number

  def initialize lines
    @lines = lines
    @line_number = 0
  end

  def [] ln
    @lines[ln]
  end

  def append ln, l
    @lines.insert ln, l
  end

  def line
    @lines[@line_number]
  end

  def line= l
    @lines[@line_number] = l
  end

  def dump
    @lines.join("\n")
  end
end

