module Xiki
  class Quick

    MENU = "
      @conf/
      - .docs/
        - keys/
          > as+quick
          | Saves current file in the @quick menu.
          > open+quick
          | Lists the current items, so you can quickly jump to them.
      "

    MENU_HIDDEN = "
      .save
      "

    def self.save

      options = yield

      target_file = options[:ancestors][-1].sub /\/$/, ''

      conf_file = File.expand_path("~/menu/conf/quick.conf")
      txt = File.read conf_file

      # If a file, use View.open...
      if File.exists? target_file
        key = target_file[/.+\/(\w+)/, 1]
        target_code = "View.open '#{target_file}'"
      else   # If a buffer, use Buffers.to...
        key = target_file
        target_code = "Buffers.to '#{target_file}'"
      end

      txt.sub! /^-/, "- #{key}/\n  ! #{target_code}\n-"

      File.open(conf_file, "w") { |f| f << txt }

      View.message "saved to @quick menu"
      View.kill if View.name == "@@quick/save/"
    end

    def self.menu_before *args
      nil
    end

    def self.menu_after output, *path

      # Don't do anything if path exists and output - already handled

      return if output && path.any?

      options = yield

      conf = options[:conf]
      conf.sub! /.+?^-/m, '-'   # Chop off stuff at top

      txt = Tree.children conf, path

      View.kill :force_recent=>1 if View.name == "@quick" && txt =~ /\A!/

      MenuHandler.eval_exclamations txt

      return "" if ! output

      # /, so add @conf at top
      if path.blank?
        # Add the items after @conf
        output.sub! /@conf\/\n/, "\\0#{txt}"
      end

      output
    end

  end
end
