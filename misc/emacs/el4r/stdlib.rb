
#
## [2005/06/27] winconf-push
@winconf_stack = []
defun(:winconf_push, :interactive=>true) do
  funcall(:message, "pushd winconf")
  @winconf_stack.push([current_window_configuration, point])
end

#
## [2005/06/27] winconf-pop
defun(:winconf_pop, :interactive=>true) do
  winconf, pt = @winconf_stack.pop
  if winconf
    set_window_configuration(winconf)
    goto_char pt
    funcall(:message, "popped winconf")
  else
    funcall(:message, "winconf_pop: winconf stack is empty")
  end
end

#
## [2005/06/27] winconf-command
defun(:winconf_command, :interactive=>"P") do |arg|
  if arg
    winconf_push
  else
    winconf_pop
  end
end

#

## / Local Variables
## / eeb-defaults: (eeel4r ee-delimiter-hash nil t t)
## / End:
