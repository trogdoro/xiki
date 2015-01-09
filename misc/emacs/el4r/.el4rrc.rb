@lisp_object_gc_trigger_count = 100
@lisp_object_gc_trigger_increment = 100
@ruby_gc_trigger_count = 100
@ruby_gc_trigger_increment = 100
@log_buffer = "*el4r:log*"
@output_buffer = "*el4r:output*"
@unittest_lisp_object_gc_trigger_count = 5000
@unittest_lisp_object_gc_trigger_increment = 5000
@unittest_ruby_gc_trigger_count = 5000
@unittest_ruby_gc_trigger_increment = 5000
@temp_file = "/var/folders/66/3h81t2y913vf344z6fb2cks00000gn/T/el4r-#{ENV['USER'] || ENV['USERNAME'] || 'me'}.tmp"

# This file is loaded once from lisp and once from ruby, so this var sometimes isn't set
XIKI_DIR = ENV['XIKI_DIR'] if ! defined?(XIKI_DIR)

@instance_program = "#{XIKI_DIR}/misc/emacs/el4r/el4r-instance"

### El4r bootstrap code
def __conf__
  if ENV['EL4R_ROOT']
    $: << File.join(ENV['EL4R_ROOT'], "lib")
  end
  require "#{XIKI_DIR}/misc/emacs/el4r/el4r-sub.rb"
  ConfigScript.new(__FILE__)
end

def __elisp_init__
  $> << "(setq \n"
  instance_variables.map{|iv| [iv[1..-1], instance_variable_get(iv)]}.each {|k,v|  $> << "el4r-#{k.gsub(/_/,'-')} #{v.inspect}\n" if Numeric === v or String === v}
  $> << ')' << "
"
end

at_exit { __elisp_init__  if __FILE__==$0 }

### Customizable variables
### You can override these variables in User-setting area.
@init_script = "init.rb"

# Emacs program name used by el4r / el4r-runtest.rb
@emacs_program = "emacs"
