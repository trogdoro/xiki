require "time"   # For .iso8601 method

# Make sure it's defined even if not in xikihub
if ! defined? XIKIHUB_DIR
  XIKIHUB_DIR = nil
end

module Xiki
  class XikihubClient

    @@session_cookie = nil

    @@server = "xiki.com"
    # @@server = "xiki.loc"
    # @@server = "xiki.us"

    def self.url options={}

      raise "Shouldn't try to access url from xikihub server" if XIKIHUB_DIR

      return @@server if options[:short]

      return "https://#{@@server}" if @@server == "xiki.com"

      "http://#{@@server}"

    end

    def self.search options

      args, option_item = options[:args], options[:task]   # ["git", "> Heading"]

      # Action, so just run local version
      if args.length > 1 && args[1] =~ /\A[a-z]/i
        return Options.propagate_important_options(options) do |options|

          # Options is now limited to ones that should be propagated downward
          Xiki.expand args[0], args[1..-1], options
        end
      end

      url = self.url

      # Handle options for root...
      if args.length == 1   #> nil
        # /~, so return option to add task...   #> nil
        return "* add note" if option_item == []

        # # /~ add note, so delegate to .__...
        return Notes.add_note_prompt if option_item == ["add note"]
      end

      # Log to list+history location

      if args.length <= 2
        Topic.append_log ":"+args.join(" ")
      end

      # Add args to url
      if args
        path_root = "/"   #> nil
        args_path = Path.join(args).gsub(' ', '-')
        url << path_root+args_path   #> nil
      end

      url = URI.encode(url)   #> "http://xiki.loc/nova/> Bang/Here are some elements;lfor you.;l;l;l"

      # If we're passing "content\n" > (if \n in final path item), so use special handling

      if option_item == ["navigate"]
        args.push "\n"
        options.delete :task   # Remove "navigate"

        url << "/;l"
      end

      if args[-1] =~ /\n/
        return self.search_when_quote url, args, options   #> |||
      end

      # Make call to server...

      result = self.get url   #> "http://xiki.loc/nova/%3E-Bang"

      # Show friendly message when server is down?

      # Result starts with {..., so parse as json
      if result =~ /\A{/
        hash = TextUtil.symbolize_hash_keys JSON[result]

        # Has reason, so show it
        return "|-Couldn't search, because: #{hash[:reason]}." if hash[:reason]
        # Otherwise, just show raw json
        return Tree.quote result
      end

      name = args[0].gsub(/ /, '_')

      if args.length == 1
        structure = TopicExpander.local_structure(name)
        TopicExpander.search_local_structure(structure)

        self.reposition_upvotes_to_top result, structure
        self.pathify_actions result, structure
      end

      # Move line down, past initial ": ..." line, if there is one
      if args.length >= 2
        if result =~ /\A: /
           options[:line_found] = 2
        elsif result =~ /\A@.+\n  : /
           options[:line_found] = 3
        end
      end

      result = ": None found." if result == ""
      result

    end

    def self.pathify_actions result, structure#, name

      # Get locally installed actions...

      installed_actions = structure.values.map{|o| o[2].keys}.flatten
      installed_actions = installed_actions.grep(/\A> @\w* \./)

      # Grab installed and shared
      installed_actions.map!{|o| o.sub(/@\w* /, '')}

      # For each, search and replace > Headings to pathified

      installed_actions.each do |heading|
        pathified = "+ #{Notes.heading_to_path heading}"
        result.gsub! /^#{Regexp.quote heading}$/, pathified
      end
    end

    def self.pathify_users_actions result, topic

      # Get locally installed actions...

      # Read Xiki File for this topic

      file = File.expand_path("~/xiki/#{topic.gsub(' ', '_')}.xiki")

      txt = File.read(file) rescue ""
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      installed_actions = txt.split("\n").grep(/^> @\w+ \./)
      installed_actions.map!{|o| o.sub(/@\w+ /, '')}

      # For each, search and replace > Headings to pathified

      installed_actions.each do |heading|
        pathified = "+ #{Notes.heading_to_path heading}"
        result.gsub! /^#{Regexp.quote heading}$/, pathified
      end

    end

    def self.reposition_upvotes_to_top result, structure#, name

      # Get locally installed actions...

      upvoted_headings = structure.values.map do |o|
        Notes.extract_upvoted_headings o[2]
      end.flatten

      upvoted_headings.map!{|o| o.sub '@ ', ''}

      result.replace "#{upvoted_headings.join "\n"}\n#{result}"
      result.replace result.split("\n").uniq.join("\n")

    end

    def self.search_when_quote url, args, options

      option_item, args = options[:task], options[:args]

      username = self.username_from_quotes

      # Username from quote, so add to url
      if username
        url = url.split("/").insert(5, "@#{username}").join("/")
      elsif args[2] =~ /^@/
        username = args[2][/@(\w+)/, 1]
      end

      is_yours = self.username == username   #> false

      # ^O, so show options for search...

      if option_item # == []
        # Get user of note

        menu = self.option_items

        xik = Xik.new(menu)

        return xik[OptionItems.prepend_asterisk(option_item), :eval=>options]
      end

      # Make initial call to server > to get actual command name (to handle multiple words)

      result = self.get url

      result = JSON[result]

      # Expanding, so call remote...

      line_orig = $el ? Line.value : nil

      # If yours
      if is_yours

        name = result['name'].gsub(' ', '_')
        filename = File.expand_path "~/xiki/#{name}.xiki"   #> For now > just go to your version of it
        View.open filename

      else
      # If other's
        # Open in unsaved view and > navigate   #> Todo > change buffer name to url!!
        View.to_buffer "#{self.url(:short=>1)}/@#{username}/#{result['name']}", :clear=>1, :dir=>"~/xiki", :after_bar=>1
        Notes.mode
        View >> result['txt']

      end

      # Move to line we were on

      heading = is_yours ? args[1].sub(/> /, "> @ ") : args[1]

      Notes.jump_to_heading heading
      Notes.jump_to_text_under_heading line_orig

      return ""

    end


    def self.option_items #options

      txt = %`
        * install
          ! XikihubClient.install options
        * upvote
          ! XikihubClient.upvote options
        * comment
          ! XikihubClient.comment options
      `.unindent

      # * web
      #   ! url = "todo > url"
      #   ! Browser.url url
      #   ! ""

    end


    def self.username_from_quotes
      tree_siblings = Tree.siblings
      status_line = tree_siblings.find{|o| o =~ /^:/}
      return if ! status_line
      status_line[/@(\w+)/, 1]
    end


    def self.search_key

      # Text is selected, so make paragraph into a quote...
      return Grab.quote_selection if View.selection?

      # Collapse first if children
      Tree.collapse if Tree.children?

      path = Tree.path
      line = Line.value

      # /, so insert "xiki/" and expand...

      if line == ""
        Tree.<< "xiki/", :no_slash=>1
        Launcher.launch
        return View.remove_last_undo_boundary   # So ^Z will undo the insert and the expand
      end

      # /:foo, so just expand...

      if line =~ /^[ =]*:[a-z][a-z0-9 ]*$/i
        return Launcher.launch
      end

      # /foo, so prepend colon and expand...

      if line =~ /^[ =]*[a-z][a-z0-9 ]*$/i
        # Collapse first (if any children)
        Tree.collapse

        Line.sub! /[a-z]/i, ":\\0"
        return Launcher.launch
      end

      # Blank shell prompt ("$ "), so delete it and do ^W again...

      if path[-1] =~ /\A\$ ?\z/

        line = Line.value
        if line =~ /^ /
          line.sub! /^( +)(.*)/, "\\1= xiki/"   # Indent, so add "=" after indent
        else
          line = "xiki/"   # No indent, so delete whole line
        end

        Line.sub! /.*/, line
        return Launcher.launch
      end

      # ^S on "$ foo", so change prompt and expand again

      if path[-1] =~ /\A[$%] /
        Line.sub! /^\$ /, "% "
        search = Topic.shell_command_to_topic(path[-1])   #> echo

        # Do search and insert results
        txt = Xiki[":#{search}"]
        Tree << txt

        return
      end


      # Do nothing when ^S on onrecognized line
      # "foo/" or "$ foo" or some other type of line
      # Tree.<< "Todo > implement what ^S on this does"

    end

    def self.user path, options
      # "@x/x > x" syntax > so add slash
      path = path.sub(/^(@\w+\/\w+) >/, "\\1/>")

      url = "#{XikihubClient.url}/#{path}"

      path = Path.split path

      # Save to list+history
      if path.length <= 3
        log_txt = path[0..1].join("/")
        log_txt << " #{path[2]}" if path[2]
        Topic.append_log log_txt
      end

      # Make call to server...

      result = X(url.sub(/.+?:\/\//, ''))
      result = ": None found." if result == ""

      topic = path[1]

      # @user/topic/, so pathify any actions we have installed

      if path.length == 2
        self.pathify_users_actions result, topic
      end

      if path.length > 2

        # Pull user off beginning and insert after heading
        user = path.shift
        path.insert 2, user

        # Put colon at the beginning of search

        path[0].sub! /^/, ':'

        # Delegate to s/h/@u/c

        Options.propagate_important_options(options) do |options|
          result = Xiki.expand path[0], path[1..-1], options
        end
        return result

      end

      result

    end

    # Return ~/xiki/hub/user/ path.   #> Todo > search for all places that use .local_hub_dir!
    # Return nil if it doesn't exist.   #> Todo > search for all places that use .local_hub_dir!
    def self.local_hub_dir user
      dir = File.expand_path "~/xiki/hub/#{user}"
      return nil if ! Dir.exists? dir
      dir
    end

    def self.user_stuff user, options

      # Get stuff from local hub dir if it exists...

      local, remote = "", ""

      if hub_dir = self.local_hub_dir(user)

        files = Dir.entries(hub_dir).select{|o| o !~ /^\.+$/}

        notes, commands = files.partition{|o| o =~ /\.xiki$/}

        local << notes.map do |o|
          o.sub(/^/, '+ :').sub(/\..+/, "/\n").gsub("_", ' ')
        end.join("")

        local << commands.map do |o|
          o.sub(/^/, '+ ').sub(/\..+/, "").gsub("_", ' ').sub(/$/, "/\n")
        end.join("")
      end

      # Get remote stuff from xikihub...

      remote = "| Todo > # Get remote stuff from xikihub...\n"

      # Local and remote, so label them separately
      if local.any? && remote.any?
        return "| Installed:\n#{local}| Not installed:\n#{remote}"
      end

      "#{local}#{remote}"   # One or the other
    end

    # def self.user_topic user, topic, items, options
    def self.user_topic path, options
      user, topic, items = Path.split path

      # If hub dir exists, see if command is locally installed...

      if hub_dir = self.local_hub_dir(user)

        # :notes, so drill in as notes...

        if topic =~ /\A:(\w)/
          topic = topic.sub(/\A:/, '').gsub(' ', '_')
          path = "#{hub_dir}/#{topic}.xiki"
          # Notes file exists, so delegate to .drill
          if File.exists? path

            # Get this working when > navigating to the file
            return Notes.drill path, *items, options
          end
        end

      end

      # Local command didn't handle it, so delegate to xikihub...

      path = path.sub(/^@/, '')
      url = "#{self.url}/#{path}"
      url = URI.encode url, "> "

      self.get url

    end

    # Loads from ~/.xikihub, caching it, and return hash containing "username" and "auth_token".
    def self.auth_file_hash= val
      @@auth_file_hash = val
    end

    def self.auth_file_hash
      @@auth_file_hash ||= self.auth_file_hash_load
    end

    def self.auth_file_hash_load
      txt = File.expand_path("~/.xikihub")
      txt = File.read(txt)
      Hash[txt.scan(/(.+): (.+)/)]

    rescue
      {}   # Return blank hash if file not found

      # ~/.xikihub
      #   : username: trogdoro
      #   : auth_token: c1073bd15cf8b8aoeuaoeu
    end

    def self.username
      return nil if XIKIHUB_DIR
      auth_file_hash = self.auth_file_hash

      auth_file_hash["username"]
    end

    def self.auth_token
      return nil if XIKIHUB_DIR

      self.auth_file_hash["auth_token"]
    end

    def self.internet_connection_exists

      # Don't check connection if connecting to the local server
      return nil if self.using_dev_xikihub?

      require "resolv" if ! defined? Resolv

      # Timeout::timeout(2.5) do
      Timeout::timeout(7) do
        dns_resolver = Resolv::DNS.new()
        address = dns_resolver.getaddress("xiki.com")
      end
      nil

    rescue Exception => e

      Ol "error connecting!!!"
      "Couldn't save to xikihub (offline or slow connection)"
    end


    # Called by DiffLog.save, and .share?
    def self.message email, subject, section, options={}

      url = "#{self.url}/.message"

      result = self.post url, :body=>{
        :email=>email,
        :subject=>subject,
        :section=>section,
      }

    end


    def self.save filename, options={}   #> .save:

      # Do nothing if not a task file (later: follow links? > maybe ignore links > like for projects with notes checked into their repo)   #> "quick"

      return if ! Notes.in_home_xiki_dir?(filename)

      topic = filename[/(\w+)\.xiki$/, 1]   #> continue here > implement new save!!!

      # Do nothing if not a .xiki file...

      return if ! topic

      txt = File.read filename
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      shared_txt = self.extract_shared txt   #> ||

      # Was blank, so delete instead

      md5 = Digest::MD5.hexdigest shared_txt
      checksums = self.hub_synced_versions_checksums   #> "d41d8cd98f00b204e9800998ecf8427e"
      basename = File.basename filename
      synced_md5, synced_local_modified = checksums[basename]   #> "foo.xiki"

      # No shared (and no previously shared), so do nothing...

      if shared_txt == ""
        # No previous checksum means no shared headings existed before, so assume the change is local only
        return if ! synced_md5
      end

      # A change to shared headings was made, so continue and call xikihub...

      # Compare hash of self.extract_shared with file?...

      synced_md5 ||= ""   # No checksum for existing shared headings, so assume new (or renamed)

      shared_was_modified = md5 != synced_md5   #> true

      # Shared part wasn't modified, so no need to save...

      if ! shared_was_modified
        # Save timestamp and md5 (should be unchanged?) into hub_synced_versions
        checksums[basename] = [md5, File.mtime(filename).utc.iso8601]
        self.persist_hub_synced_versions_checksums checksums

        return
      end

      username = self.username   #> "trogdoro"

      if ! username
        View.open :txt=>%`
          - You don't have a XikiHub account:
           Since this command has headings like "> @ Foo", they would
           normally be shared to XikiHub when you save. But since you
           don't have an account, they couldn't be. They were saved
           locally for now.

          - Set up an account:
          Go here to set up an account:
          http://xiki.com

          + close/
        `.unindent, :no_search=>1, :line_found=>10
        return ""
      end

      $el.el4r_fork_and_eval "Xiki::XikihubClient.save_async '#{filename}', #{options.inspect}".inspect, "Xiki::XikihubClient.save_async_completed"

      nil

    end


    #
    # Takes a while, so only called via .el4r_fork_and_eval().
    #
    def self.save_async filename, options={}   #> .save_async:

      if result = self.internet_connection_exists   #> # Update file (after save to remote)!!
        return result
      end

      topic = filename[/(\w+)\.xiki$/, 1]   #> continue here > implement new save!!!
      basename = File.basename filename   #> "foo"

      topic_hyphens = topic.gsub('_', '-')

      # No username, so raise error instead of trying to save...

      username = self.username

      url = "#{self.url}/@#{username}/#{topic_hyphens}"   #> "http://xiki.loc/@trogdoro/zaa"
      url = URI.encode url   #> "http://xiki.us/@trogdoro/foo"

      txt = File.read filename
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      shared_txt = self.extract_shared txt   #> ||

      post = JSON.pretty_generate({   #> !
        :txt=>shared_txt,
        :auth_token=>self.auth_token,
        :force=>1,
      })

      result = self.post url, :body=>post   #> {"status":"success"}

      # Return error hash if result doesn't look like json   #> {"status"=>"success"}
      return {:status=>"error", :returned_txt=>result}.to_json if result !~ /\A[{\[]/   #> {"status"=>"success"}
      result = JSON[result]

      if result["error"]
        return result["reason"] if result["reason"]
        return result.to_json
      end

      # Update checksum file after save
      checksums = self.hub_synced_versions_checksums   #> "foo.xiki"
      md5 = Digest::MD5.hexdigest shared_txt   #> returning success!
      checksums[basename] = [md5, File.mtime(filename).utc.iso8601]
      self.persist_hub_synced_versions_checksums checksums   #> returning success!

      # Was a newly created file, so pop open browser window

      if options[:open_browser]
        Browser.url url, :os_open=>1
      end

      return "Updated #{url.sub(/.+?:\/\//, '')}"

    rescue Exception=>e

      Ol "error in async!!!"
      Ol "e.message", e.message
      Ol.a e.backtrace

      e.message

    end

    def self.save_async_completed

      response = $el.elvar.el4r_fork_and_eval_output

      if response == nil
        raise "- el4r_fork_and_eval_output was nil!!!"
      end

      response.chomp!("\0")

      # Save succeded, so do nothing...
      if response == '"success"'   #> true
        return View.message "Xikihub save succeeded"
      end

      # Error, so show dialog...

      View.create_horizontal
      response.sub!(/^\"(.+)\"$/, "\\1")
      View.open :txt=>"> Issue while saving to xikihub\n- #{response}\n\nok/\n" #, :line_found=>4
      Move.bottom
      Move.up

    end

    # Returns hash from hub_synced_versions/checksums.txt file.
    #
    #   "foo.xiki" => ["md5ofsharedpart", "f_localdatemodified"],
    #   "bar.xiki" => ["md5ofsharedpart", "b_localdatemodified"]
    def self.hub_synced_versions_checksums

      filename = File.expand_path "~/.xiki/misc/hub_synced_versions/checksums.txt"
      checksums = File.read(filename) rescue nil

      return {} if ! checksums

      checksums.strip.split("\n").inject({}) do |ret, o|
        key, md5, date = o.split(/ +/)
        ret[key] = [md5, date]
        ret
      end

    end

    def self.persist_hub_synced_versions_checksums checksums

      dir = File.expand_path "~/.xiki/misc/hub_synced_versions"
      FileUtils.mkdir_p dir   # Make sure it exists
      filename = "#{dir}/checksums.txt"

      blank_md5 = Digest::MD5.hexdigest("")
      checksums = checksums.delete_if{|o| checksums[o][0] == blank_md5}

      txt = checksums.map do |k, v|
        "#{k}  #{v.join("  ")}"
      end.join("\n")

      File.open(filename, "w") { |f| f << txt }

    end



    def self.out_of_sync_view diffs

      # Identical if > only the 2 top lines in the diff
      identical = diffs.scan(/\n/).length == 2
      if identical
        View.open(%`
          - No differences - you've gotten them in sync!

          = close/
        `.unindent, :name=>"out of sync")#, :line_found=>9
      else
        View.open(%`
          > Oops, you've gotten out of sync
          You have two different versions of this note:
            :-XikiHub version
            :+Local version

          Type Ctrl+X in the diff below to navigate. After you've gotten your
          local version how you want it, expand "update xikihub" below.

        `.unindent+"\n#{diffs}\n    = update xikihub/\n", :name=>"out of sync")#, :line_found=>9
      end

      Search.forward "^ *= "
      Move.backward 2

    end


    def self.auth

      # Make call to /_auth...

      url = "#{self.url}/_auth"

      # Call /auth even if no username?
        # Because why? > We want to have a session even when > No username
          # Wait > could we just have no session mean anonymous?
            # No > because we no session might mean we just haven't authed yet (or re-authed after a session timeout)
            # And > a session for anonymous users will probably be useful in the future
        # What happens now when anonymous? > No call to auth

      options = {:body=>{:username=>self.username, :auth_token=>self.auth_token}}

      options[:raise] = 1
      result = RestTree.post url, options

      self.raise_if_not_json result, "Couldn't authenticate with xikihub."

      result = TextUtil.symbolize_hash_keys JSON[result]

      # Incorrect username or password supplied, so stop now and show error

      if result[:result] != "success"

        # Clear out cached <username etc, so we'll reload from the file after they change it
        @@auth_file_hash = nil

        View.open("
          > Incorrect username or password for XikiHub
          - 1) Correct your username and auth token here
          ~/.xikihub
            : username:
            : auth_token:

          - 2) Reload xiki
            | By typing Alt+R (or Meta+R)

          + close/
        ".unindent)
        View.line = 4
        View.column = 3

        raise ""
      end

      cookies = options[:response].get_fields('set-cookie')

      # Server is starting a new session, so store the cookie so we can always pass it back...

      session_cookie = cookies.find{|o| o =~ /^_xikihub_rails_session=/}
      if session_cookie
        value = session_cookie[/=(.+?);/, 1]

        # Auth succeeded (and they are at least confirmed) so store session cookie
        @@session_cookie = value
      end


    rescue SocketError=>e
      raise "Couldn't authenticate with xikihub. Check your internet connection."
    end


    def self.raise_if_not_json json, extra_message=nil
      return if json =~ /\A\{/

      message = "Check your internet connection. Response from server was strange."
      message = "#{extra_message} #{message}" if extra_message

      raise message
    end

    def self.using_dev_xikihub?
      self.url =~ /\.loc$/
    end


    def self.request url, options={}

      # We don't have a session cookie yet, so call auth...

      max_time = self.using_dev_xikihub? ? 5 : 12   #> 5
      result = nil

      Timeout::timeout(max_time) do   #> ||||||

        if ! @@session_cookie
          self.auth   #> ||||||
        end

        options[:headers] = {"User-Agent"=>"Xsh", "Cookie"=>"_xikihub_rails_session=#{@@session_cookie}"}   #> |||
        options[:raise] = 1

        result = options[:action] == "get" ?
          RestTree.get(url, options) :
          RestTree.post(url, options)

        # Server is telling us we're unconfirmed (maybe our session expired?) so call auth...

        # Maybe switch over to using this custom HTTP header field:
        # - WWW-Authenticate: Xiki requested   < or "Xiki required"
          # No field needed for the client to pass > but here's one that would probably not be blocked > Authorization:

        cookies = options[:response].get_fields('set-cookie')
        if cookies && cookies.find{|o| o == "request_auth=1"}
          self.auth   # Save session_cookie if success   #> ||||||
        end

        # 401 means it didn't do the action, so auth and then call again...

        if options[:response].code == 401
          self.auth   # Save session_cookie if success
          result = options[:action] == "get" ?
            RestTree.get(url, options) :
            RestTree.post(url, options)
        end

      end

      result

    rescue Errno::ECONNREFUSED=>e

      Ol "#{e.message}!!!"
      Ol.a e.backtrace

      url = self.url #.sub(%r".+?//", '')
      return %`{"error":true, "reason":"We couldn't connect to #{url}. Are you connected to the internet?"}`

    rescue Exception=>e   #> "http://xiki.loc/quick-fruit"

      Ol "#{e.message[0..1000]}!!!"   #> A JSON text must at least contain two octets!!!!
      Ol.a e.backtrace

      url = self.url.sub(%r".+?//", '')
      return %`{"error":true, "reason":#{e.message.inspect}}`
    end

    def self.get url, options={}

      options[:action] = "get"

      if url =~ /;l/
        split = Path.split url
        if split[-1] =~ /\n/
          options[:action] = "post"
          options[:body] = split.pop
          url = Path.join split
        end
      end

      self.request url, options
    end

    def self.post url, options={}
      options[:action] = "post"
      self.request url, options   #> |||||
    end

    def self.extract_shared txt
      txt = Notes.split txt   #> |||
      # Remove "> Private" headings
      txt.delete_if{|o| o =~ /\A(>$|> $|> (?!@ ))/i}
      txt.each{|o| o.sub! /^> @ /, "> "}

      txt.join("")
    end

    def self.dir_modified_times options={}
      tasks_dir = options[:dir] || "~/xiki"
      tasks_dir = File.expand_path tasks_dir
      txt = ""
      Dir.entries(tasks_dir).sort_by(&:downcase).each do |f|
        next if f =~ /^\./ || f !~ /\.xiki$/   # Skip .foo and foo.unknownextension
        txt << "#{File.mtime("#{tasks_dir}/#{f}").to_i} #{f}\n"
      end
      txt
    end

    def self.delegate_to_save_async file
      $el.el4r_fork_and_eval "Xiki::XikihubClient.save_async '#{file}'".inspect, "Xiki::XikihubClient.save_async_completed"
    end

    def self.web

      file = View.file

      # If command file
      if file && Notes.in_home_xiki_dir?(file)

        topic = File.basename(file, ".*").gsub('_', '-')
        url = "#{self.url}/@#{self.username}/#{topic}"
        # Just show in browser
        Browser.url url, :os_open=>1

      elsif ! file

        # In the future, when no username show "go here to join private beta" teaser
        return View.flash("- Requires XikiHub username!") if ! XikihubClient.username

        Notes.save_on_blank :task=>["share"]

      else

        # Else > some unsaved buffer probably

        View.open :txt=>"
          - implement!!!
          # File that's not in commands dir > how to handle?
        "

      end

    end


    def self.install options

      args = options[:args]
      args = args[0..-2] if args[-1] =~ /\n/   # Remove last arg if \n

      user = self.username_from_quotes   #> "http://xiki.loc/quick/> Fruit"
      args.insert(2, "@#{user}") if user


      url = "#{XikihubClient.url}/#{args.join '/'}"   #> continue here > implement upvote!!
      url = URI.encode(url.gsub(" ", '-'))

      # Call server to get name
      result = self.post url, :body=>{:action=>"install"}   #> continue here > implement upvote!!
      result = JSON[result]

      if result["name"]

        file = File.expand_path "~/xiki/#{result["name"].gsub(' ', '_')}.xiki"
        txt = File.read(file) rescue ""

        txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

        heading = args[1].sub(/>/, "> @#{result['user']}")

        Notes.replace_section txt, heading, Tree.unquote(result['txt'])
        File.open(file, "w") { |f| f << txt }

        # If action > collapse and put'+ foo' above

        if args[1] =~ /^> \./ && Line =~ /^ +[:|]/
          # Move up to heading and collapse it
          Tree.to_parent while Line !~ /^ +> /
          indent = Line.indent

          Line.to_left
          left = View.cursor
          Tree.after_children
          right = View.cursor
          View.cursor = left
          Effects.glow :fade_out=>1, :what=>[left, right], :delay=>0.06

          View.delete left, right

          # Insert action version of this line above it
          View >> "\n"
          View >> "#{indent}+ #{Notes.heading_to_path args[1]}"
          Line.to_end
          Effects.glow :fade_in=>1, :delay=>0.08

          return ""
        end

        return "<* - installed task!"
      end

      Tree.quote result.ai   # Show error

    end


    def self.upvote options

      args = options[:args]
      args = args[0..-2] if args[-1] =~ /\n/   # Remove last arg if \n

      # Insert @user from quote into url if there...

      user = self.username_from_quotes
      args.insert(2, "@#{user}") if user

      url = "#{XikihubClient.url}/#{args.join '/'}"   #> continue here > implement upvote!!
      url = URI.encode(url.gsub(" ", '-'))

      result = self.post url, :body=>{:action=>"determine_name"}

      return self.show_error(result) if result !~ /\A{/

      result = JSON[result]

      # Unsuccessful response, so just show...

      command = result["name"]
      if ! command
        return Tree.quote result.ai
      end

      # Successful response

      user ||= args[2][/@(\w+)/, 1] if args[2] =~ /^@/   #> "nova bang"
      heading = args[1].sub(/^> /, '> @ ')


      file = File.expand_path "~/xiki/#{command.gsub(' ', '_')}.xiki"
      txt = File.read(file) rescue ""
      self.merge_upvote_into_command txt, heading, user

      File.open(file, "w") { |f| f << txt }

      # Server will increment upvote count if appropriate
      self.delegate_to_save_async file

      # Move it to top and fade in...
      self.upvote_move_to_top

      return ""

    end


    def self.upvote_move_to_top

      orig, orig_line, orig_column = View.cursor, View.line, View.column

      # Move up to parent
      while(Line !~ /^ *>/)
        Tree.to_parent
      end

      # Previously isn't indented, so there's no where to move
      already_at_top = Line.value(0) !~ /^ /

      # Subtract lines, so we know how far down > to put the cursor when finished
      orig_line -= View.line

      bounds = Tree.item_bounds

      # Fade out > and delete

      View.cursor = orig

      Effects.glow(:fade_out=>1, :delay=>0.04, :what=>bounds) if ! already_at_top
      txt = View.delete *bounds

      # Increment the points
      txt.sub!(/^ +: (\d+ points|@\w+, \d+ points)/) do |o|
        points = o[/\d+/]
        points = (points.to_i+1).to_s
        o.sub /\d+/, points
      end

      # Move to top

      Tree.to_parent
      Move.down

      # Insert at top > And fade in

      left = View.cursor
      right = left + txt.length
      View >> txt

      View.column = orig_column
      Move.down orig_line

      if already_at_top
        return View.flash "- upvoted!"
      end

      Effects.glow :fade_in=>1, :delay=>0.06, :what=>[left, right]

    end


    def self.comment options

      option_items = options[:task]

      # ~ comment, so prompt them to enter comment...

      if option_items.length == 1
        options[:nest] = 1
        options[:no_search] = 1
        options[:line_found] = 2
        options[:column_found] = 2
        return "
          : Enter your comment here, then type Ctrl+O:
          | 
          |
        ".unindent
      end

      # ~ comment/The Comment, so add it to your command...

      comment = option_items[1]

      # Lookup name on server > skip this if one-word search string?

      args = options[:args]
      args = args[0..-2] if args[-1] =~ /\n/   # Remove last arg if \n

      if args[2] =~ /^@/
        # Extract out in case it was in the path before
        user = args[2][/@(\w+)/, 1]
      else
        user = self.username_from_quotes
        args.insert(2, "@#{user}")   #> Restructerd this > Might cause problems!
      end

      #> Test when > username in path vs quotes

      url = "#{XikihubClient.url}/#{args.join '/'}"   #> continue here > implement upvote!!
      url = URI.encode(url.gsub(" ", '-'))   #> "{\"user\":\"trogdoro\",\"name\":\"pool\"}"
      result = self.post url, :body=>{:action=>"determine_name"}
      result = JSON[result]
      if result["name"]

        file = File.expand_path "~/xiki/#{result["name"].gsub(' ', '_')}.xiki"
        txt = File.read(file) rescue ""
        heading = args[1].sub(/>/, "> @")
        user = result['user']

        self.merge_comment_into_command txt, heading, user, comment

        File.open(file, "w") { |f| f << txt }

        # Find method to pass filename > async
        XikihubClient.delegate_to_save_async file

        return "<* - commented!"
      end

      Tree.quote result.ai   # Show error

    end


    # def self.task username, command, heading
    def self.task args
      url = "#{XikihubClient.url}/@#{args.join '/'}"   #> continue here > implement upvote!!
      url = URI.encode(url.gsub(" ", '-'))   #> "http://xiki.loc/nova/> Bang/Here are some elements;lfor you.;l;l;l"
      self.get url
    end


    def self.comment_or_upvote_label_regex user, options={}

      if options[:comments]
        return "^(@#{user} [^\n +].*|@#{user}:|@#{user} \\+1 \\S.*|@#{user} \\+1:)$"
      elsif options[:upvotes]
        return "^@#{user} \\+1$"
      end

      "^(@#{user} [^\n +].*|@#{user}:|@#{user} \\+1 \\S.*|@#{user} \\+1:|@#{user} \\+1)$"
        # "^(Upvote for @#{user}\\.|Comment for @#{user}:|Upvote and comment for @#{user}:)$"
    end


    def self.merge_upvote_into_command txt, heading, user

      # For reference > the 3 label syntaxes
      #   Upvote for @nov.
      #   Comment for @nov:
      #   Upvote and comment for @nov:

      # Get existing section if any...
      section = Notes.extract_section(txt, heading)

      # Section doesn't exist yet, so create empty one...

      if ! section
        txt.replace "#{heading}\n#{txt}"
        section = ""
      end

      label_regex = self.comment_or_upvote_label_regex user
      label = section[/#{label_regex}/]

      # No upvote label yet, so add it at top...

      section = "#{section}@#{user} +1\n\n" if ! label

      # Merge in upvote...

      if label =~ /^@\w+(:| \S.*)$/
        after_username = $1
        # Existing comment label, so change to upvote and comment
        section.sub! /#{label_regex}/, "@#{user} +1:"
      end

      # Delete if there and add to top instead

      Notes.delete_section txt, heading
      Notes.prepend_section txt, "#{heading}\n#{section}"

      txt

    end

    def self.show_error txt
      View.open :txt=>"> Response from xikihub\n#{txt}"
    end

    def self.merge_comment_into_command txt, heading, user, comment

      # For reference > the 5 label syntaxes
      #   @nov comment
      #   @nov:\nmultiline comment
      #   @nov +1
      #   @nov +1 comment
      #   @nov +1:\nmultiline comment

      # old:
      #   Upvote for @nov.
      #   Comment for @nov:
      #   Upvote and comment for @nov:

      comment = "#{comment.strip}"

      # Get existing section if any...

      section = Notes.extract_section(txt, heading)

      # Section doesn't exist yet, so create empty one...

      if ! section
        txt.replace "#{heading}\n#{txt}"
        section = ""
      end

      # Look for "upvoted @x" or "comment on @x" section...

      label_regex = self.comment_or_upvote_label_regex user
      label = section[/#{label_regex}/]

      # Simple upvote label, so merge comment in
      if label =~ /\A@\w+ \+1\z/
        if comment =~ /\n/
          section.sub! /^#{Regexp.quote label}$/, "\\0:\n#{comment}"
        else
          section.sub! /^#{Regexp.quote label}$/, "\\0 #{comment}"
        end
      elsif label =~ /\A@\w+ \+1:\z/

        section.sub! /^#{Regexp.quote label}$/, "\\0\n#{comment}\n"

      # No comment, or just normal existing comment
      else

        # For now, just append when there was existing
        # Since I don't know whow I'll handle multiple comments yet
        if comment =~ /\n/
          section = "#{section}@#{user}:\n#{comment}\n\n"
        else
          section = "#{section.strip}\n\n@#{user} #{comment}\n\n"
        end
      end

      Notes.replace_section txt, heading, section
      txt

    end

  end
end
