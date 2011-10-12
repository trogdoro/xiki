require 'net/http'
require 'uri'
require 'timeout'
require 'keys'

class Redmine
  extend ElMixin

  @@url ||= 'http://foo.com:80/wiki/1'
  def self.url= to;  @@url = to;  end
  def self.url;  @@url;  end

  def self.menu
    puts "
      + .pages/
      - .start
      - local: http://localhost:3401/wiki/gateway
      - remote: http://rlitio.chase.com:3401/wiki/1
      "
  end

  def self.pages page=nil
    if page  # If page was passed, open it
      self.open page
      return
    end

    # Otherwise, print list
    t = self.http_get "#{@@url}/Page_index/special" rescue return
    t.scan(/<a href="\/wiki\/1\/([\w-]+)"/) { |m| puts "- #{m[0]}" }
    ""
  end

  def self.redmine_to_xiki txt

    # Angle brackets
    txt.gsub!('&lt;', '<');  txt.gsub!('&gt;', '>')
    txt.gsub!('&amp;', '&');  txt.gsub!('&quot;', '"')

#     puts txt
#     return

#     (1..7).to_a.reverse.each do |i|  # Headers (h1. etc)
#       txt.gsub!(/^h#{i}\. /, "#{'|' * i} ")
#     end
    (1..7).to_a.reverse.each do |i|  # Headers
      txt.gsub!(/^#{'#' * i}/, "#{'|' * i} ")
    end
    (1..9).to_a.reverse.each do |i|  # Bullets
      txt.gsub!(/^#{'\\*' * i}/, "#{'  ' * (i-1)}-")
    end

    txt
  end

  def self.xiki_to_redmine txt
    # Headers (h1. etc)
    #(1..7).to_a.reverse.each { |i| txt.gsub!(/^#{'\\|' * i} /, "h#{i}. ") }
    (1..7).to_a.reverse.each { |i| txt.gsub!(/^#{'\\|' * i} /, "#{'#' * i}") }
    (1..9).to_a.reverse.each { |i| txt.gsub!(/^#{'  ' * (i-1)}-/, "#{'*' * i}") }
    txt
  end

  def self.open name

    # TODO grab token into a buffer name
    version = "no_version"
    wiki, version = self.page_from_server name
    self.redmine_to_xiki wiki
    View.to_after_bar
    View.to_buffer("redmine/#{name}/#{version}")

    # Set mode and use relevant key shortcuts
    notes_mode;  use_local_map elvar.redmine_mode_map
    self.apply_styles

    if View.empty?  # If nothing there yet, insert
      insert wiki
    end
    Move.top
    ""
  end

  def self.page_from_server name

    t = self.http_get "#{@@url}/#{name}/edit" rescue
      return
    version = t[/<input id="content_version".+value="(\d+)"/, 1]
    wiki = t[/<textarea.*?>(.+)<\/textarea>/m, 1]
    wiki.gsub!("\C-m", '')
    [wiki, version]
  end

  def self.save
    name, version = View.buffer_name.split('/')[1..2]
    txt = View.txt  # Get text from buffer
    return puts("- Not a redmine page!") unless txt
    self.xiki_to_redmine txt

    # Get text from server
    server_txt, server_version = self.page_from_server name

    # If version is same, save
    if version == server_version
      res = Net::HTTP.post_form(URI.parse("#{@@url}/#{name}/edit"), {  # Save
        'content[version]' => version, 'content[text]' => txt
      })
      message "- page saved"
      $el.rename_buffer "redmine/#{name}/#{version.to_i + 1}"  # Rename
    else  # Otherwise show diff
      message "- wrong version, show diffs!"
      View.to_buffer "*redmine_collision: #{name}/#{server_version}"
      notes_mode;  use_local_map elvar.redmine_mode_map

      View.clear
      $el.insert "- collision with #{name}/#{server_version}!\n\n"
      $el.insert self.diff(server_txt, txt)
    end
  end

  def self.http_get url
    t = nil
    begin
      timeout(2) do
        t = Net::HTTP.get URI.parse(url)
      end
    rescue Exception => e
      puts "- Couldn't connect to url (#{url}): #{e.message}!"
      raise e
    end
    t
  end

  def self.init
    unless boundp :redmine_mode_map
      # Create new map
      elvar.redmine_mode_map = make_sparse_keymap
      # Inherit notes_mode_map!
      set_keymap_parent elvar.redmine_mode_map, elvar.notes_mode_map
    end
    Keys.XS(:redmine_mode_map) { Redmine.save }

    # Make C-. follow link
    Launcher.add(/\[\[.+\]\]/) do |line|  # Redmine wiki links
      name = line[/\[\[(.+?)\]\]/, 1]
      name.gsub!(/ /, "_")
      Redmine.open(name)
    end
  end

  def self.diff a, b
    temp_path_a = "/tmp/diff_tmp_a.txt"
    temp_path_b = "/tmp/diff_tmp_b.txt"
    File.open(temp_path_a, "w") { |f| f << a }
    File.open(temp_path_b, "w") { |f| f << b }
    diff = $el.shell_command_to_string "diff -w -U 0 \"#{temp_path_a}\" \"#{temp_path_b}\""
    DiffLog.format("server/", "diff/", diff)
  end

  def self.apply_styles
    # Strikethrough
    Styles.define(:strike, :strike => true)
    Styles.apply("^ *- \\(-.+-\\)$", nil, :strike)
    # _italic_
    Styles.define(:redmine_green, :bold => true, :fg => "33dd33")
    Styles.apply(" _[a-zA-Z0-9 ]+_ ", :redmine_green)
    Styles.apply(" _[a-zA-Z0-9 ]+_$", :redmine_green)

  end

  def self.start
    Rails.start '$o/redmine/trunk', @@url[/\d+/]
  end

end
# Redmine.init
