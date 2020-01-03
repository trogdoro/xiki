module Xiki
  class Shoes
    def self.menu *args
      # txt = ENV['txt']
      txt = args[0]

      app = "/Applications/Shoes.app/Contents/MacOS/shoes"

      return "
        > Shoes isn't installed
        <@ shoes/docs/install shoes/
        " if ! File.exists? app

      File.open("/tmp/shoes.rb", "w") { |f| f << txt }

      `#{app} /tmp/shoes.rb`
    end
  end
end
