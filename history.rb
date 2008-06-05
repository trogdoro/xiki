class History
  extend ElMixin

  def self.prefix_times
    prefix = Keys.prefix
    prefix ||= 20
    prefix = 20 if prefix == :u
    prefix
  end

  def self.open_current options={}
    if options[:prompt_for_bookmark]
      bm = Keys.input(:timed => true, :prompt => "Enter bookmark to show outline for: ")
      path = Bookmarks.expand(bm, :just_bookmark => true)
        #Files.directory? Bookmarks.expand("h", :just_bookmark => true)
      paths = [path]
    elsif options[:outline] || options[:all]
      paths = [buffer_file_name(buffer_list[0])]
    else  # No options passed
      times = Keys.prefix
      paths = ( buffer_list.map { |b| buffer_file_name(b) }.select{|path| path})
      paths = paths[0..(times-1)] if times  # Limit to number if prefix passed
    end

    if options[:enter_here]  # If entering in current file
      path = paths.to_s
      # If it's a dir, delegate to Open Tree
      if path =~ /\/$/
        TreeLs.ls :here => true, :dir => path
        return
      end
      insert TreeLs.filename_to_next_line(paths.to_s)
      open_line 1
      TreeLs.enter_lines

    else  # If entering in new buffer

      View.bar if options[:bar]  # If to go to bar
      #View.bar
      View.to_buffer("*tree of current")
      View.clear;  notes_mode
      insert TreeLs.paths_to_tree(paths)
      View.to_top
      Keys.clear_prefix
      TreeLs.select_next_file
      if options[:all]
        TreeLs.enter_lines(//)
      elsif options[:outline] || options[:prompt_for_bookmark]
        TreeLs.enter_lines
      else
        TreeLs.search :recursive => true
      end
    end

  end

  def self.open_edited
#     if Keys.prefix_u
#       cm_show_edited_files
#     else
    times = self.prefix_times
    #View.bar
    View.to_buffer("*tree of edited")
    View.clear;  notes_mode
    insert TreeLs.paths_to_tree(elvar.editedhistory_history.to_a[0..(times-1)])
    View.to_top
    Keys.clear_prefix
    TreeLs.select_next_file
    TreeLs.search :recursive => true
#     end
  end

  def self.open_history
    times = self.prefix_times
    View.to_buffer("*tree of history")
    View.clear;  notes_mode

    self.insert_history times
    View.to_top
    Keys.clear_prefix
    TreeLs.select_next_file
    TreeLs.search :recursive => true
  end

  def self.insert_history times
    insert TreeLs.paths_to_tree(elvar.recentf_list.to_a[0..(times-1)])
  end

  def self.enter_history

    orig = Location.new
    self.insert_history self.prefix_times
    right = point
    orig.go
    TreeLs.search :recursive => true, :left => point, :right => right

  end

  def self.open_unsaved

    # Get unsaved paths
    buffer_modified_p

    # Narrow down to modified buffer only
    modified = buffer_list.to_a.
      select{|b| buffer_modified_p(b)}.
      select{|b| buffer_file_name(b)}.
      map{|b| buffer_file_name(b)}

    View.to_buffer("*tree of unsaved buffers")
    View.clear;  notes_mode
    if (modified.size == 0)
      return insert("| Note\n- ~No Buffers Unsaved~\n")
    end
    insert TreeLs.paths_to_tree(modified)
    View.to_top
    Keys.clear_prefix
    TreeLs.select_next_file
    TreeLs.search :recursive => true
  end

end
