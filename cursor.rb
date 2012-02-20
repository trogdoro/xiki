class Cursor

  @@remember = {}

  def self.menu
    %`
    > Summary
    | Api for changing the cursor
    |
    - Colors/
      @Cursor.white
      @Cursor.red
      @Cursor.green
      @Cursor.blue
      @Cursor.color "#80f"
    - Shapes/
      @Cursor.bar
      @Cursor.underscore
      @Cursor.hollow
      @Cursor.box
    - Colors and Shapes/
      @Cursor.red_bar
      @Cursor.green_underscore
      @Cursor.blue_hollow
      @Cursor.black_box
    - Remembering and restoring cursor/
      @Cursor.remember :a
      @Cursor.restore :a
    `

  end

  def self.bar
    $el.el4r_lisp_eval "(customize-set-variable 'cursor-type '(bar . 2))"
    nil
  end
  def self.box
    $el.customize_set_variable :cursor_type, :box
    nil
  end
  def self.underscore
    $el.el4r_lisp_eval "(customize-set-variable 'cursor-type '(hbar . 3))"
    nil
  end
  def self.hollow
    $el.customize_set_variable :cursor_type, :hollow
    nil
  end

  def self.color color
    $el.set_face_background :cursor, color
    nil
  end

  def self.blue
    $el.set_face_background :cursor, "#0099ff"
    nil
  end
  def self.red
    $el.set_face_background :cursor, "#ff3300"
    nil
  end
  def self.green
    $el.set_face_background :cursor, "#33bb00"
    nil
  end
  def self.white
    $el.set_face_background :cursor, "#ffffff"
    nil
  end
  def self.black
    $el.set_face_background :cursor, "#000000"
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

  def self.remember symbol
    # Save is hash for later restoring (only if not there yet)
    @@remember[symbol] = [$el.elvar.cursor_type, $el.face_background(:cursor)]
  end

  def self.restore symbol
    before = @@remember[symbol]
    return Cursor.black_box unless before  # Black if not found
    type, color = before
    $el.set_face_background :cursor, color
    $el.customize_set_variable :cursor_type, type
  end

end

