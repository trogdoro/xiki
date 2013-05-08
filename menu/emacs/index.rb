module Foo
  class Emacs
    def self.info_pages name
      View.to_upper
      $el.info "(emacs)#{name}"
      nil
    end
  end
end

