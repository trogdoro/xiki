module Xiki
  class Cursor

    @@remember = {}

    def self.menu
      %`
      - summary/
        | Api for changing the cursor
      - colors/
        =Cursor.white
        =Cursor.red
        =Cursor.green
        =Cursor.blue
        =Cursor.color "#80f"
      - shapes/
        =Cursor.bar
        =Cursor.underscore
        =Cursor.hollow
        =Cursor.box
      - colors and shapes/
        =Cursor.red_bar
        =Cursor.green_underscore
        =Cursor.blue_hollow
        =Cursor.black_box
      - remembering and restoring cursor/
        =Cursor.remember :a
        =Cursor.restore :a
      `

    end

    def self.bar
      return if ! Environment.gui_emacs
      $el.el4r_lisp_eval "(customize-set-variable 'cursor-type '(bar . 4))"
      nil
    end
    def self.box
      return if ! Environment.gui_emacs
      $el.message "*"
      $el.customize_set_variable :cursor_type, :box
      nil
    end
    def self.underscore
      return if ! Environment.gui_emacs
      $el.el4r_lisp_eval "(customize-set-variable 'cursor-type '(hbar . 3))"
      nil
    end
    def self.hollow
      return if ! Environment.gui_emacs
      $el.customize_set_variable :cursor_type, :hollow
      nil
    end

    def self.color color=nil

      return if ! Environment.gui_emacs
      return $el.face_background(:cursor) if color.nil?

      Styles.define :cursor, :bg=>color
      nil
    end

    def self.blue
      Styles.define :cursor, :bg=>"0099ff"
      nil
    end
    def self.red
      Styles.define :cursor, :bg=>"ff3300"
      nil
    end
    def self.green
      Styles.define :cursor, :bg=>"33bb00"
      nil
    end
    def self.white
      Styles.define :cursor, :bg=>"ffffff"
      nil
    end
    def self.black
      Styles.define :cursor, :bg=>"000000"
      nil
    end

    def self.red_bar
      self.red
      self.bar
    end
    def self.blue_hollow
      self.blue
      self.hollow
    end
    def self.green_underscore
      self.green
      self.underscore
    end
    def self.blue_underscore
      self.blue
      self.underscore
    end
    def self.black_box
      self.black
      self.box
    end

    def self.remember symbol=:default
      return if ! Environment.gui_emacs
      # Save is hash for later restoring (only if not there yet)
      @@remember[symbol] = [$el.elvar.cursor_type, $el.face_background(:cursor)]
    end

    def self.restore symbol=:default
      return if ! Environment.gui_emacs
      before = @@remember[symbol]
      return Cursor.black_box unless before  # Black if not found
      type, color = before
      $el.customize_set_variable :cursor_type, type
      nil
    end

  end
end
