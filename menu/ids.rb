class Ids
  def self.menu *args

    # If just "ids/", list all...

    if args.blank?
      js = %`
        var result="";
        $('*[id]').each(function(i, e){
          var id = $(e).attr('id');
          result += "+ #"+id+"/\\n";
        })
        result;
        `.unindent

      result = Firefox.exec js
      return "
        > No id attributes found!
        | Maybe try listing by classes:
        << css/list/
        " if result == '""'
      return result
    end

    # Id passed so just navigate to it...

    Xiki["dom", args]

  end
end
