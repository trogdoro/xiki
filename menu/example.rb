module Xiki
  # Wraps @eval, because "example" looks more appropriate
  # in some cases, like:
  #
  # foo/docs/counting/
  #   @example/
  #     | p "123"
  class Example
    def self.menu *args
      return "@beg/neighbors/" if args[-1] !~ /\n/
      txt, out, exception = Code.eval args[-1].gsub(/^\| ?/, ''), :pretty_exception=>1
      return exception if exception
      Tree.quote out || txt
    end
  end
end
