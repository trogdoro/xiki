load "#{Xiki.dir}commands/rails.rb" if ! defined?(Xiki::Rails)

module Xiki
  class Models
    MENU_OBSCURED = %`
      - */
        @r/<name>.new
        @r/<name>.last
        @r/<name>.all
        - .more/
          - one record/
            @r/<name>.find 1
          - certain fields/
            @r/<name>.all.map{|o| o.name}
          - sort/
            @r/<name>.order("name")
            @r/<name>.order("name").map{|o| o.name}
          - columns/
            @r/<name>.columns
          - .delete/
            - one record/
            - all records/
            - model/
      `
    MENU = %`
      - .relationships/
      `

    def self.delete_records name
      "- foo!"
    end

    def self.delete name, kind

      dir = Rails.running_dir
      name = TextUtil.camel_case name

      if kind == "one record"
        "@r/#{name}.delete 1"
      elsif kind == "all records"
        "@r/#{name}.delete_all"
      elsif kind == "model files"
        "
        @#{dir}/
          % rails destroy model #{name}
        "
      end

    end

    def self.menu_after output, *path

      # /, so show list...

      if path.blank?
        txt = Rails.run_in_app 'Dir.new("#{Rails.root.to_s}/app/models").entries.grep /\.rb$/'

        return txt if txt =~ /\A\|/
        txt = YAML::load txt
        #         txt = txt.map{|o| "#{o[/(.+)\.rb$/, 1]}/\n"}.join("")
        txt = txt.map{|o| "+ #{o[/[\w ]+/]}/\n"}.join("")
        return "#{txt}#{output}"
      end

      return self.relationships(yield) if path == ["relationships"]

      # /foo/, so populate <name> in output...

      model = TextUtil.camel_case path[0]
      output.gsub "<name>", model

    end

    def self.relationships options # name=nil

      options[:no_search] = 1

      dir = Rails.running_dir
      models_dir = "#{dir}/app/models/"
      txt = "
        @#{models_dir}
          - ##^ *(has_|belongs_to )/
        "
    end

  end
end
