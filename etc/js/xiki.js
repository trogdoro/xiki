exports.children = function(tree, target) {

  target = target.replace(/^\//, '');
  target = target.replace(/\/$/, '');

  var found=null, result="";
  if(target == "") found = -1;

  exports.traverse(tree, function(branch, path){

    if(found == null) {
      var target_match = exports.target_match(path, target);

      if(target_match == 'same' || target_match == 'same'){
        found = branch.length - 1;   // Remember indent
      }

    }else{

      current_indent = branch.length - 1;

      // If found and still indented one deeper
      if(current_indent == found + 1) {
        var item = branch[branch.length-1];
        if(item.match(/\/$/)) item = item.replace(/^- /, '+ ');
        item = item.replace(/^([+-] )?\./, "$1")
        result += item+"\n";   // Output
      }else{   // Otherwise, stop looking for children if indent is less
        if(current_indent <= found) found = null;
      }
    }

  });

  return result;
};


exports.target_match = function(path, target) {

  var pi=0, ti=0;

  while(true){
    var pathi=path[pi], targeti=target[ti];

    if(! pathi || ! targeti){
      if(!pathi && !targeti) return 'same';   // Handles a, a and a/, a/
      if(!pathi && !target[ti+1] && targeti == "/") return 'same';   // Handles a, a/
      if(!targeti && !path[pi+1] && pathi == "/") return 'same'   // Handles a/, a
      if(pathi && (pathi == "/" || path[pi-1] == "/" || pi == 0)) return 'shorter'
      if(targeti && (targeti == "/" || target[ti-1] == "/" || ti == 0)) return 'longer'
      return null;   // At end of one, but no match
    }

    if(pathi == targeti){   // If chars equal, increment
      pi += 1;
      ti += 1;
      continue;
    }else if(pathi == "." && (path[pi-1] == "/" || pi == 0) && (target[ti-1] == "/" || ti == 0)){
      pi += 1;
      continue;
    }

    break;   // Not found
  }

  return null;
}

exports.traverse = function(tree, callback) {
  var branch=[], indent=0;

  tree = tree.split("\n");
  for(var i=0; i < tree.length; i++) {
    var line = tree[i];
    if(line == "") continue;
    var line_indent = String(line.match(/^ */)).length / 2;

    line = line.replace(/^ */, '');

    branch[line_indent] = line;

    if(line_indent < indent) {
      branch = branch.slice(0, line_indent+1);
    }

    var path = branch.join('').replace(/(^|\/)[+-] /g, "$1");

    callback(branch, path);

    indent = line_indent;
  }
}

exports.render_mobile = function(tree, target) {

  return '\
    <html>\n\
      <head>\n\
        <meta name="viewport" content="width=device-width, initial-scale=1">\n\
        <link rel="stylesheet" href="http://code.jquery.com/mobile/1.0rc1/jquery.mobile-1.0rc1.min.css" />\n\
        <script src="http://code.jquery.com/jquery-1.6.4.min.js"></script>\n\
        <script src="http://code.jquery.com/mobile/1.0rc1/jquery.mobile-1.0rc1.min.js"></script>\n\
      </head>\n\
    \n\
      <body>\n\
        <div data-role="page">\n\
          <div data-role="header">\n\
            <h1>Menu</h1>\n\
            <a rel="external" href="index.html" data-icon="arrow-l" class="ui-btn-left">Back</a>\n\
            <a rel="external" href="index.html" data-icon="home" class="ui-btn-right">Home</a>\n\
          </div>\n\
          <div data-role="content">\n\
            <ul data-role="listview" data-theme="g">\n\
              <li><a rel="external" href="cars.html"></a></li>\n\
              <!--\n\
              <li><a rel="external" href="boats.html">Boats</a></li>\n\
              <li><a rel="planes" href="#" onclick="alert(\'hi\')">Planes</a></li>\n\
                -->\n\
            </ul>\n\
          </div>\n\
        </div>\n\
      </body>\n\
    \n\
      <script>\n\
        path = "";\n\
    \n\
        function back(e) {\n\
          path = path.replace(/(.*)\\/.*/, "$1");\n\
          // console.log(path);\n\
          fetch(path);\n\
          return false;\n\
        }\n\
    \n\
        function home(e) {\n\
          path = "";\n\
          fetch(path);\n\
          return false;\n\
        }\n\
    \n\
        function go(e) {\n\
          // console.log(e.target);\n\
          var item = $(e.target).html().toLowerCase()\n\
          // console.log(item);\n\
          path += "/"+item;\n\
          fetch(path);\n\
          return false;\n\
        }\n\
    \n\
        function fetch(path) {\n\
          console.log(123)\n\
          $.get(path, {}, function(txt){\n\
            $("li:first").siblings().remove();\n\
    \n\
            $.each(txt.split("\\n"), function(i, t){\n\
              if(! t) return;\n\
              var item = t.match(/\\w.+/)[0];\n\
              item = item.replace(/\\/$/, "");\n\
              item = item.replace(/\\b\\w/g, function(m){return m.toUpperCase()});\n\
              // console.log(item);\n\
              // If not first, clone\n\
              if(i > 0){\n\
                $("ul").append($("li:eq(0)").clone());\n\
              }\n\
              var li = $("li:last");\n\
              li.find("a").html(item);\n\
            })\n\
            $(".ui-btn-active").removeClass("ui-btn-active");\n\
            var title = path.replace(/.*\\//, "");\n\
            if(! title) title = "Menu";\n\
            title = title.replace(/\\b\\w/g, function(m){return m.toUpperCase()});\n\
            $(".ui-title").html(title);\n\
          });\n\
        }\n\
    \n\
        $(function(){\n\
        fetch("");\n\
          $("li a").live("click", go);\n\
          $(".ui-btn-left").live("click", back);\n\
          $(".ui-btn-right").live("click", home);\n\
        });\n\
      </script>\n\
    \n\
    <html>\n\
    '

}
