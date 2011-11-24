# Helps you define modes.
#
# Sample usage:
#   Mode.define(:deck, ".deck") do
#     Deck.apply_styles
#   end
#
class Mode

  def self.menu
    "
    - Put something here.
    "
  end

  # Easy way to define a mode
  def self.define name, extension, &block

    # Mode function
    $el.defun("#{name}_mode".to_sym, :interactive => "", :docstring => "Apply #{name} styles, etc") do
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"
      block.call

      map_name = "#{name}_mode_map".to_sym
      $el.use_local_map $el.elvar.send(map_name) if $el.boundp(map_name)
    end

    # Associate with file extension
    $el.el4r_lisp_eval %Q<(add-to-list 'auto-mode-alist '("\\\\#{extension}\\\\'" . #{name}-mode))>

  end
end
