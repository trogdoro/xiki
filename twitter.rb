require 'net/http'
require 'timeout'

class Twitter
  extend ElMixin

  CODE_SAMPLES = %q<
    # C-. on this
    - Twitter.menu
  >

  def self.post_or_list
    if Keys.prefix_u  # If U prefix, just list
      shell_command("twitter timeline")
    else  # Otherwise, post
      self.post
    end
  end

  def self.list options={}
    View.handle_bar
    Shell.run "twitter timeline", :buffer => "*twitter timeline"
    Styles.define :twitter_name, :fg => '99f'
    Styles.define :twitter_date, :fg => 'ddd'
    Styles.apply '^\\(-- .+\\)\\( at .+\\)', nil, :twitter_name, :twitter_date
    View.to_top
    kill_line
    View.previous
  end

  def self.tree options={}
    txt = nil
    begin
      timeout(3) do
        txt = Net::HTTP.get URI.parse("http://twitter.com/statuses/friends_timeline/13453802.atom")
      end
    rescue Exception => e
      return puts("- Timed out!")
    end

    txt.scan( /<title>(.+)<\/title>/ ).each do |t|
      puts "- #{t}"
    end
    View.wrap :off
    ""
  end

  def self.post message
    puts shell_command_to_string("twitter post \"#{message}\"")
  end

  def self.menu options={}
    puts "+ in tree: .tree/"
    puts "- async: .list"
    puts "- post: .post 'foo'"
  end

end
