# It's a url (1st args is blank), so delegate to http://... passing "~ source"...

# Construct url
url = "http:/#{args.join("/")}"

Xiki[url, :task=>["source"]]


# cla## Http
#   def self.menu *args
#     yield[:dont_log] = 1
#     url = args.blank? ? nil : args.join('/')
#
#     # If as+open, just jump to the log
#     return View.open Launcher.log_file if Keys.open?
#
#     # If no url's, list them all...
#
#     if url.blank?
#       txt = Launcher.last "http", :exclude_path=>1
#       txt.gsub! /^- /, '<< '
#       return txt
#     end
#
#     Keys.prefix == :u ? $el.browse_url(url) : Firefox.url(url)
#     nil
#   end
# end
