class Xiki::DropdownHandler
  include Xiki

  def self.handle options

    source = options[:handlers]['dropdown']
    return if ! source || options[:output] || options[:halt]

    # Better api call for this?  Maybe pass 'ex' in through options as well, so we don't need the param.

    source = "#{options[:enclosing_source_dir]}#{source}"


    txt = File.read(source)
    txt = "#{txt.strip}\n"
    txt_with_code = txt.dup

    txt.gsub! /^ *!.+\n/, ''   # remove !... lines

    Line.add_slash :no_move=>1
    View.refresh

    cursor = Line.left + Line.indent.length

    result = Xiki::Menu.dropdown txt, :cursor=>cursor, :offset=>[25, 19], :no_root=>1

    code = Tree.children txt_with_code, result

    return options[:output] = result if ! code

    code.gsub! /^! /, ''

    txt, out, exception = Code.eval code, source, 1, :pretty_exception=>1

    return exception if exception

    txt = txt.to_s rescue "couldn't parse result"
    txt = out || txt
    if txt.any?   # Don't quote if blank line
      txt = Tree.quote txt
      options[:output] = txt
    end

  end

end
