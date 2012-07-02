class Urls
  def self.menu *args
    url = args.blank? ? nil : args.join('/')

    # If as+open, just jump to the log
    return View.open Launcher.log_file if Keys.open?

    # If no url's, list them all

    return Launcher.last "urls", :exclude_path=>1 if url.blank?

    Keys.prefix == :u ? $el.browse_url(url) : Firefox.url(url)
    nil
  end
end
