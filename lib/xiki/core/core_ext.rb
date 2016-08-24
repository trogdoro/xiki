require 'xiki/core/text_util'

class Array
  def blank?
    self.empty?
  end

  def snippet
    Xiki::Tree.pipe to_s
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
    self.replace Xiki::TextUtil.unindent(to_s)
  end

  def snippet
    Xiki::Tree.pipe to_s
  end

  def quoted
    Xiki::Tree.quote to_s
  end

  def no_trailing
    to_s.gsub(/ +$/, '')
  end



  def camel_case
    Xiki::TextUtil.camel_case(to_s)
  end
  def snake_case
    Xiki::TextUtil.snake_case(to_s)
  end
  def hyphen_case
    Xiki::TextUtil.hyphen_case(to_s)
  end
  def word_case
    Xiki::TextUtil.word_case(to_s)
  end


end

class Hash
  def snippet
    Xiki::Tree.pipe to_s
  end
end

class NilClass
  def snippet
    "nil"
  end
end
