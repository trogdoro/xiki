module Xiki
  #
  # Lists themes and run them. Make files here
  # to make more themes:
  #
  # =:xiki/misc/themes/
  #
  class Themes

    def self.use name # , options={}

      path = "#{Xiki.dir}misc/themes/#{name.gsub " ", "_"}.notes"
      raise "- Theme doesn't exist!" if ! File.exists? path

      load path

      if ControlLock.enabled?
        $el.control_lock_apply_bar_color
      end

      nil
    end

    def self.init_in_client

      $el.el4r_lisp_eval %`
        (defun xiki-bar-add-padding (txt &rest format-space)
          (let ((total (window-width)) left right)
            (setq left (- total (length txt)))
            (setq left (/ left 2))
            (when (> 0 left) (setq left 2))
            (setq right (- total (+ left (length txt))))
            (when (> 0 right) (setq right 2))
            (if format-space
              (concat
                (propertize (make-string left ? ) 'face 'mode-line-sides)   ; Make string of spaces
                txt
                (propertize (make-string right ? ) 'face 'mode-line-sides)   ; Make string of spaces
              )
              (concat
                (make-string left ? )   ; Make string of spaces
                txt
                (make-string right ? )   ; Make string of spaces
              )
            )
          )
        )
      `

    end

  end
end
