# /, so list all dirs

links = Notes.extract_links :limit=>2000

links.map!{|o| "= #{File.dirname o[0]}/\n"}
links.uniq!
links.join

