class Eval
  def self.menu *args
    # If any args, just eval them
    if args.any?
      txt = ENV['txt']

      cursor = View.cursor
      Tree.to_parent
      parent_line = View.line
      View.cursor = cursor

      txt, out, exception = Code.eval txt, View.file, parent_line+1

      return CodeTree.draw_exception exception, txt if exception
      return out.any? ? out : txt.to_s
    end

    "
    | # Put some ruby code here to eval it, such as:
    | p 1 + 2
    "
  end
end
