class Http
  def self.menu *args
    url = args.blank? ? nil : args.join('/')

    # If as+open, just jump to the log
    return View.open Launcher.log_file if Keys.open?

    # If no url's, list them all

    if url.blank?
      txt = Launcher.last "http", :exclude_path=>1
      txt.gsub! /^- /, '<< '
      return txt
    end

    Keys.prefix == :u ? $el.browse_url(url) : Firefox.url(url)
    nil
  end
end
