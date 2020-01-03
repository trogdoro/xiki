module Xiki
  class Block

    def self.do_as_wrap

      line = Line.value

      # If |... line, indent consecutive |... lines...

      if quote = line[/^ *([|:#]|\/\/) /, 1]

        orig = View.cursor

        bounds = Tree.sibling_bounds :quotes=>quote

        txt = View.delete bounds[0], bounds[-1]

        indent = Line.indent txt
        txt.gsub! /^ *#{Regexp.quote quote} ?/, ''

        # For each paragraph, wrap separately...

        txt = txt.split "\n\n"
        txt.map! do |paragraph|
          paragraph.gsub!(/ *\n */, ' ')   # Remove linebreaks
          paragraph = TextUtil.word_wrap(paragraph, 64-indent.length).strip
          paragraph
        end
        txt = txt.join "\n\n"

        txt = Tree.quote txt, :char=>quote
        txt.gsub! /^/, indent

        View << txt+"\n"
        insert_right = bounds[0] + txt.length
        orig = insert_right-1 if orig > insert_right
        View.cursor = orig

        return

      elsif Keys.prefix_u?
        # Grab paragraph and remove linebreaks

        orig = Location.new
        txt = View.paragraph :delete => true, :start_here => true
        txt.gsub! "\n", " "
        txt.sub!(/ $/, "\n")
        View.insert txt
        orig.go

        Line.to_left
        View.insert "> "
        $el.fill_paragraph nil
        txt = View.paragraph(:delete => true)
        View.insert txt.gsub(/^  /, '> ')
        return
      end

      cursor = View.cursor
      paragraph = View.paragraph :bounds=>1
      $el.fill_region_as_paragraph Line.left, paragraph[1]
      View.cursor = cursor

    end

    #
    # Convenience method for keys like do+as+coffee.
    #
    # It...
    # 1. grabs the text from current section (or per prefix),
    # 2. invoked the block, passing in the text
    # 3. inserts the output (under a ">>" heading)
    #
    # Block.do_as_something do |txt|
    #   CoffeeScript.execute txt
    # end
    #
    def self.do_as_something

      prefix = Keys.prefix
      txt, left, right = View.txt_per_prefix prefix

      result = yield txt

      # Insert result at end of block
      orig = Location.new
      View.cursor = right
      Line.to_left

      if prefix.nil?
        View.insert(">>\n"+result.strip+"\n") unless result.blank?
      else
        View.insert(result.strip.gsub(/^/, '  ')+"\n") unless result.blank?
      end

      orig.go
    end

    def self.>> txt
      orig = Location.new
      ignore, left, right = View.block_positions "^>"
      View.cursor = right

      View.insert(">>\n#{txt.strip}\n") unless txt.blank?
      orig.go
    end

  end
end
