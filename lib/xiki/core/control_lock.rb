module Xiki
  # Simple wrapper around control_lock.el to turn
  # it on and off (and handle the case where it's not)
  # installed.
  class ControlLock

    def self.toggle
      $el.control_lock_enable
    end

    def self.disable
      $el.control_lock_enable if self.enabled?
    end

    def self.enabled?
      $el.boundp(:control_lock_mode_p) and $el.elvar.control_lock_mode_p
    end
  end
end
