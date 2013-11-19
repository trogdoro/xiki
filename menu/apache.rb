module Xiki
  class Apache
    MENU = %`
      - .restart/
      - .control/
        - .restart/
        - stop/
          ! Console.run "sudo apachectl stop", :dir=>"/etc/apache2/"
        - start/
          ! Console.run "sudo apachectl start", :dir=>"/etc/apache2/"
      - conf/
        @/etc/apache2/
          + other/
          + httpd.conf
      - logs/
        @/var/log/apache2/
          % tail -f error_log
          % tail -f access_log
      `

    def self.restart
      Console.run "sudo apachectl restart", :dir=>"/etc/apache2/"

      "- restarting!"
    end
  end
end
