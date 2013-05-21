require 'xiki/core/text_util'

class Array
  def blank?
    self.empty?
  end
end

class NilClass
  def empty?
    true
  end

  def blank?
    true
  end

  def any?
    false
  end
end

class String
  def blank?
    empty?
  end

  def present?
    ! empty?
  end

  def any?
    ! empty?
  end

  def unindent
    Xiki::TextUtil.unindent(to_s)
  end
  def unindent!
    self.replace TextUtil.unindent(to_s)
  end
end
