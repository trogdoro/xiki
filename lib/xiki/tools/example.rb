module Xiki
  # Wraps @eval, because "example" looks more appropriate
  # in some cases, like:
  #
  # foo/docs/counting/
  #   @example/
  #     | p "123"
  class Example
    def self.menu *args
      Eval.menu *args
    end
  end
end
