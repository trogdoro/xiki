require 'xiki/text_util'

class Array
  def blank?
    self.empty?
  end
end

class NilClass
  def empty?
    self.nil?
  end

  def blank?
    self.nil?
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
    TextUtil.unindent(to_s)
  end
end
