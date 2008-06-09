class Memorize

  def self.start
    # Get block and clean
    block = Notes.block
    block.gsub! "'", "\\\\'"
    block.sub! /\A/, "| From memorize.notes\n"

    # Insert into db
    RubyConsole[:ml].run("
      m = Main.find_or_create({:name => 'mem'})
      m.update_attributes(:display_name => 'Mem', :txt => '#{block}')
      ")
    $el.browse_url "http://localhost:4001/mem"
  end

end

