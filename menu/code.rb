module Xiki
  # Wraps @eval, because "code" looks more appropriate
  # in some cases, like:
  #
  # foo/docs/counting/
  #   @code/
  #     | p "123"
  class Code
    def self.menu *args
      return "@beg/neighbors/" if args[-1] !~ /\n/

      txt, out, exception = Code.eval args[-1].gsub(/^\| ?/, ''), :pretty_exception=>1
      return exception if exception
      Tree.quote out || txt
    end
  end
end
