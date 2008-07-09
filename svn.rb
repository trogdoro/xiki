require 'net/http'

class Svn
  extend ElMixin
  def self.menu action=nil, *path
    if action == "list"
      orig = View.window
      View.to_after_bar
      out = shell_command_to_string("svn list")
      View.to_window orig
      puts out.gsub(/^/, '- ')
      return
    end

    puts "+ list/"
  end


  def self.commit comment=""
    puts CodeTree.siblings
  end

  def self.jump_to_diff
    find = Line.without_indent
    View.handle_bar
    View.to_buffer "*shell: svn diff*"
    View.to_top
    $el.re_search_forward "^Index: #{find}"
    Line.to_left
    $el.recenter(0)
  end
end
