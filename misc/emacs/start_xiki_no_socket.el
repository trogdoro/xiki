; Just set a variable and load start_xiki.el in this dir...

(setq xiki-no-socket t)
(load (replace-regexp-in-string "_no_socket.el$" ".el" load-file-name))
(message "")   ; Suppress message showing it loaded the above file
