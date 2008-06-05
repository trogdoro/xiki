require 'auto_menu'

class Help

  CODE_SAMPLES = %q<
    # Show docs
    - Code samples: Docs.display
  >

  def self.menu
    puts "
      - .keys/
      - .docs
      - .classes/
      "
  end

  def self.keys type=nil, prefix=nil, command=nil

    unless type  # No type, so display list
      puts "
        - all/
        - intermediate/
        - basic/
        "
      return
    end

    unless prefix   # No prefix, so show them
      puts "
        - As...
        - Open...
        - Enter...
        - Do...
        - To...
        - Layout...
        "
      return
    end

    # If prefix clicked on, show all under it
    unless command
      # Get matches in bindings file
      txt = Bookmarks.read("$xiki/key_bindings.rb")

      letter = prefix[0..0]
      txt.scan(/^ +Keys\.#{letter}(.+)# (.+)/) { |l|

        next if l[0] =~ /\(:/  # Skip if a local map
        comment = l[1]

        if (type == "basic/")  # If :basic, only show if 2 asterixes
          next unless comment =~ / \*\*$/
        elsif (type == "intermediate/")  # If :intermediate, only show if 1 asterixe
          next unless comment =~ / \*\*?$/
        end

        comment.sub!(/ \*+$/, '')  # Remove asterixes

        # Print, omitting details
        puts "- #{comment.sub(/:.+/, '')}"
      }
      return
    end

    puts "
      - .description
      - .source
      - .run
      "
    return

  end

  def self.classes category=nil
    # Show all, if none clicked on
    unless category
      # For each class in xiki dir (use $xiki bookmark)
      Dir.foreach(Bookmarks.expand('$xiki')) do |f|
        next unless f =~ /\.rb$/
        f.sub!(/\.rb$/, '')  # Remove extension
        f = TextUtil.camel_case f
        puts "- #{f}"
      end
      return
    end

    # Show menu
    puts "
      - .docs
      - .open
      - .methods
      "
  end

#   def self.docs category=nil, full=nil
#     View.handle_bar
#     Docs.display
#   end

  def self.open category=nil, full=nil
    puts "- implement!"
  end

  def self.run category=nil, full=nil
    puts "- implement!"
  end

  def self.description category=nil, full=nil
    puts "- implement!"
  end

  # Display docs (sample_usage) for all casses
  def self.docs name=nil

    buffer_name = "*Xiki Docs*"

    # If they gave a name, just jump to it
    if name
      View.handle_bar
      if View.buffer_open? buffer_name
        View.to_buffer buffer_name
      else
        self.display_docs
      end
      View.to_buffer buffer_name
      View.to_top
      Search.forward "^\| #{name}$"
      View.recenter_top
      return
    end

    orig = View.buffer

    View.bar unless View.bar?
    View.to_after_bar

    outline = self.display_docs

    View.to_nth 0
    #View.to_buffer orig
    puts outline

  end

  def self.display_docs
    View.to_buffer("*Xiki Docs*")
    View.clear
    $el.notes_mode

    classes = []
    # Get classes with CODE_SAMPLES
    ObjectSpace.each_object(Class) do |c|
      begin
        tmp = c::CODE_SAMPLES
        #puts "|| #{c.name}\n#{tmp.size}\n"
        classes << c
      rescue
      end
    end
    classes.sort! {|a,b| a.name <=> b.name}

    $el.insert "
      | Summary
      This is the documentation for classes in the xiki framework.

      The following is the main entry point for the documentation.  Double click on it, or move your cursor down to it and press C-. to expand it:

        - Xiki.menu

      - To run most samples:
        - Type C-. while on a line below to run the samples.

      - To run multi-line samples:
        - Type C-d C-r while on one of the lines.


      ".unindent

    outline = "- Summary\n"

    # Get classes that have doc string
    classes.each do |c|
      outline << "- #{c}\n"
      docs = c::CODE_SAMPLES.strip
      docs.gsub!(/^    /, '')
      docs.gsub!(/^# /, '|| ')
      $el.insert "| #{c}\n#{docs}\n\n\n"
    end
    View.to_top

    outline  # Return outline in case caller wants to display it

  end

end
