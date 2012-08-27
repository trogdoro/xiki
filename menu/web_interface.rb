class WebInterface

  def self.menu
    %`
    > Summary
    How to set up this url, to use Xiki menus from your browser:
    @http://xiki

    - install/
      - Mac/
        - 1. Configure Apache/
          @/etc/apache2/other/
            - Create conf file) % sudo touch xiki.vhost.conf
            - xiki.vhost.conf
              - Make writable) @chmod/777/
              - Add contents (as+update):
              | NameVirtualHost *:80
              |
              | AddHandler cgi-script .rb
              |
              | <VirtualHost *:80>
              |   ServerName xiki
              |   ServerAlias menu
              |   DocumentRoot "#{Xiki.dir}etc/www/public"
              |   ErrorDocument 503 "/error.html"
              |
              |   RewriteEngine On
              |   RewriteCond %{REQUEST_URI} !^/error.html$
              |   RewriteRule ^/(.*) http://localhost:8161/$1 [P]
              |
              |   <directory "#{Xiki.dir}etc/www/public">
              |     Order deny,allow
              |     Allow from 127.0.0.1
              |   </directory>
              | </VirtualHost>
              - Restore) @chmod/644/
        - 2. Update hosts file/
          @/etc/hosts
            - Make writable) @chmod/777/
            - Add these two lines:
            | 127.0.0.1       xiki
            | 127.0.0.1       menu
            - Restore) @chmod/644/
        - 3. Restart Apache/
          @apache/restart/
    - start/
      @$xiki/etc/www/
        % ruby -rubygems sinatra_server.rb
      @http://localhost:8161
      @http://xiki
    `
  end

end
