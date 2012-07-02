class Browser
  def self.menu
    "
    - config/
      | This file has a list of browsers
      - @ ~/xiki/config/browser.notes
    - api/
      | This class lets you choose a default browser.
      |
      | Calls to Browser.url etc. will be delegated to the default browser.
    - docs/
      > Keys
      | do+load+browser:  Reload the browser.
      |
      > See
      | Many things have yet to be pulled out of firefox.rb and made generic.
      << firefox/
      |
    "
  end

  def self.url url, options={}
    Firefox.url url, options
  end


  def self.html html
    Firefox.html html
  end

  def self.js txt, options={}
    Firefox.run txt, options
  end


  def self.open_in_browser

    file = FileTree.tree_path_or_this_file

    mappings = Menu.menu_to_hash "/Users/craig/menus/url_mappings.menu"

    result = nil
    mappings.each do |k, v|
      break file.sub!(v, "#{k}/") if file.start_with? v
    end

    # If path starts with any of the mappings, apply mapping

    if file =~ /^\//   # If no ^/, must be a url, so use http://
      file = "file://#{file}"
    else
      file = "http://#{file}"
      file.sub! /\/index.html/, '/'
    end

    self.url file
  end

end
