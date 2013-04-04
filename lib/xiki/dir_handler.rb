class DirHandler
  def self.handle options, ex
    return if ! ex['/'] || options[:output]

    items = Dir.new("#{options[:last_source_dir]}#{ex['/']}").entries.grep /^[^.]/

    items.each{|o| o.sub! /(.+)\..+/, "\\1"}   # no extensions
    items.uniq!   # no dups
    items.delete "index"   # no indexs
    items.map!{|o| "+ #{o}/\n"}   # make + foo/\n

    return if items.empty?
    items = items.join("")
    options[:output] = items

  end
end
