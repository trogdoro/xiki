module Xiki
  class DirHandler
    def self.handle options
      # Does one of these amount to non-existing dir?

      source = options[:handlers]['/']

      # This will soon be refactored to be called by hash

      return if ! source || options[:output] || options[:halt]

      # Don't handle if there was extra path beyond the dir...

      return if options[:args].any?

      items = Dir.new("#{options[:enclosing_source_dir]}#{source}").entries.grep /^[^.]/

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
