module Xiki
  class DirHandler
    def self.handle options
      # Does one of these amount to non-existing dir?

      source = options[:handlers]['/']

      # This will soon be refactored to be called by hash

      return if ! source || options[:output] || options[:halt]

      # Don't handle if there was extra path beyond the dir...

      return if options[:args].any?

      # Get dir contents, sorted by date
      items = Dir.glob("#{options[:enclosing_source_dir]}#{source}/*", File::FNM_DOTMATCH).
        select {|i| i !~ /\/\.(\.*|svn|git)$/}#.   # Exclude some dirs (exclude entensions here too?)

      # Sort, handling error when certain type of file
      items = items.sort{|a, b|
        a_time = File.mtime(a) rescue nil
        b_time = File.mtime(b) rescue nil
        next 0 if ! a_time || ! b_time
        b_time <=> a_time
      }



      items.delete "default.conf"   # ignore default conf

      items.map!{|o| File.basename(o, ".*")}   # no extensions

      items.uniq!   # no dups
      items.delete "index"   # no indexes
      items.map!{|o| "+ #{o}/\n"}   # make + foo/\n

      return if items.empty?

      items = items.join("")
      options[:output] = items

    end
  end
end
