# Read list from the file

file = File.expand_path "~/xiki/misc/logs/recent.notes"
txt = File.read file

# Reverse
txt = txt.split("\n").reverse

# Don't do this, since recently-opened ones would be after them all
# Prepend currently open, since they'll be in the right order?
txt = Buffers.list.map{|b| $el.buffer_file_name(b)}.compact + txt

txt = txt.uniq.join("\n")
txt.gsub(/(.+)\/(.+)/, "=\\1/\n  - \\2")
