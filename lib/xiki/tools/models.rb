module Xiki
  class Models
    def self.menu
      "
      - .list/
      - .relationships/
      "
      #     - structure/
      #     - diagram/
    end

    def self.list
      dir = Projects.default
      models_dir = "#{dir}app/models/"
      models = Dir.new(models_dir).entries.select{|o| o !~ /^\./}
      models.map{|o| "@r/#{o[/\w+/].camelize}.first"}
    end

    def self.relationships
      dir = Projects.default
      models_dir = "#{dir}app/models/"
      txt = "
        @#{models_dir}
          - ##^ *(has_|belongs_to )/
        "
      Tree.<< txt, :no_search=>1
      Line.next
      Launcher.launch
      nil
    end

  end
end
