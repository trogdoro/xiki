# Read list from the file

file = File.expand_path "~/xiki/misc/logs/opened_files_log.notes"
txt = File.read file

# Reverse
txt = txt.split("\n").reverse

# Don't do this, since recently-opened ones would be after them all
# Prepend currently open, since they'll be in the right order?
#txt = Buffers.list.map{|b| $el.buffer_file_name(b)}.compact + txt
# Is there an elisp event for switching to a view?
#   - maybe use that instead?

txt = txt.uniq.join("\n")
txt.gsub!(/(.+)\/(.+)/, "=\\1/\n  - \\2")

# Can't add message at top, because it messes up the search
#   - add option to tell it what kind of search it should do?
#"- Files opened in xiki recently (in this session and pass sessions)\n#{txt}"

txt

