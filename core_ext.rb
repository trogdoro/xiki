require 'text_util'

class String
  def unindent
    TextUtil.unindent(to_s)

  end
end
