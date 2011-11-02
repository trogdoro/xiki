# represents an overlay and has finder methods for getting overlays
# for more, see http://www.gnu.org/software/emacs/elisp/html_node/Overlays.html
# represents an elisp overlay.  the Overlay class finder methods for getting overlays
#-- IMPORTANT NOTES:
#   start/end have been renamed to left/right respectively
class Overlay

  def initialize(elisp_overlay)
    # raise TypeError.new("argument must be elisp overlay") if overlayp(elisp_overlay)
    @overlay = elisp_overlay
  end

  # returns all overlays that contain at least one character between left and right
  # empty overlays are included
  def self.between(left, right)
    overlays = $el.overlays_in(left, right).to_a
    overlays.map { |o| Overlay.new(o) }
  end

  # returns all overlays for current buffer
  def self.all
    between(View.top, View.bottom)
  end

  # returns all overlays that have left and right exactly
  def self.on(left, right)
    between(left, right).select { |o| o.left == left && o.right == right }
  end

  # returns all overlays that cover pos
  def self.at(pos)
    overlays = $el.overlays_at(pos).to_a
    overlays.map { |o| Overlay.new(o) }
  end

  def self.overlays_with(left, right, properties = {})
    # implementation needed
  end

  def self.find_or_make(left, right)
    overlays = on(left, right)
    if overlays.any?
      puts "warning: more than 1 overlay" if overlays.size > 1
      overlays.first
    else
      make(left, right)
    end
  end

  # maybe subclass NamedOverlay
  # what about uniqueness of name?
  def self.find_overlay_by_name(name)
    # search overlays for all overlays with property name
  end

  def self.remove_overlays
    # implementation needed
  end

  # Code Sample:  Overlay.face :trailing_whitespace, :what=>:line
  # Apply face to region
  def self.face(face, options={})
    left ||= options[:left]
    right ||= options[:right]
    if options[:what] == :line or left.nil?
      left, right = Line.left, Line.right+1
    end

    o = Overlay.find_or_make(left, right)
    o[:face] = face
    o
  end

  # returns the buffer that overlay belongs to. It returns nil if overlay has been deleted.
  def buffer
    $el.overlay_buffer @overlay
  end

  # returns a copy of the property list.
  def properties
    $el.overlay_properties @overlay
  end

  # returns the position at which overlay starts, as an integer.
  def left
    $el.overlay_start @overlay
  end

  # returns the position at which overlay ends, as an integer.
  def right
    $el.overlay_end @overlay
  end

  # deletes overlay. The overlay continues to exist as a Lisp object,
  # and its property list is unchanged, but it ceases to be attached
  # to the buffer it belonged to, and ceases to have any effect on
  # display.
  # A deleted overlay is not permanently disconnected. You
  # can give it a position in a buffer again by calling move-overlay.
  def delete
    $el.delete_overlay @overlay
  end

  # def move
  # end

  def to_elisp
    @overlay
  end

  # method missing candidate.  name space with 'set' or 'propset'
  def invisible
    $el.overlay_get(@overlay, :invisible)
  end

  def invisible=(arg)
    if arg
      $el.overlay_put(@overlay, :invisible, true)
    else
      $el.overlay_put(@overlay, :invisible, nil)
    end
  end

  def isearch_open_invisible_temporary
    $el.overlay_get(@overlay, :isearch_open_invisible_temporary)
  end

  def isearch_open_invisible_temporary=(arg)
    if arg
      $el.overlay_put(@overlay, :isearch_open_invisible_temporary, true)
    else
      $el.overlay_put(@overlay, :isearch_open_invisible_temporary, nil)
    end
  end

  def []= key, val
    $el.overlay_put(@overlay, key, val)
  end
end
