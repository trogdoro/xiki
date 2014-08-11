Xiki = {};
if(typeof(exports) == "undefined"){
  exports = Xiki;
}

exports.children = function(tree, target) {
  console.log("target:", target);

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

exports.unindent = function(txt) {

  txt = txt.replace(/^\n/, '');   // Delete initial blank line if there

  // If 1st line has no indent and 2nd line has indent (at least 3 spaces)
  if(!txt.match(/^ /) && txt.match(/^.+\n(   +)/)){
    txt = txt.replace(/^\n/, '');   // Delete initial blank line if there
    var indent = txt.match(/^.+\n(   +)/)[1];
    return txt.replace(new RegExp("^"+indent, "gm"), '');
  }

  var old_indent = txt.match(/^ */);   // Get indent of first line
  txt = txt.replace(new RegExp("^"+old_indent, "gm"), '');   // Delete current indent
  txt = txt.replace(/\n+$/g, '');

  return txt;
}

exports.header_html = function(title) {

  return '<div data-role="header">\n\
    <h1>'+title+'</h1>\n\
    <a data-rel="back" href="#" data-icon="arrow-l" class="ui-btn-left">Back</a>\n\
    <a href="#home" data-icon="home" class="ui-btn-right">Home</a>\n\
  </div>';

}

exports.render_mobile_standalone = function(tree, target) {
  return '\
    <div data-role="page">\n\
      '+exports.header_html("Menu")+'\n\
      <div data-role="content">\n\
      </div>\n\
    </div>\n\
    '
}

exports.render_mobile_ajax = function(tree, target) {

  return '\
    <html>\n\
      <head>\n\
        <meta name="viewport" content="width=device-width, initial-scale=1">\n\
        <link rel="stylesheet" href="http://code.jquery.com/mobile/1.0/jquery.mobile-1.0.min.css" />\n\
        <script src="/js/xiki.js"></script>\n\
        <script src="http://code.jquery.com/jquery-1.6.4.min.js"></script>\n\
        <script src="http://code.jquery.com/mobile/1.0/jquery.mobile-1.0.min.js"></script>\n\
      </head>\n\
      <body>\n\
        <div data-role="page">\n\
          '+exports.header_html("Menu")+'\n\
          <div data-role="content">\n\
          </div>\n\
        </div>\n\
      \n\
        <script>\n\
          $(function(){\n\
            Xiki.ajax_setup();\n\
          })\n\
        </script>\n\
      </body>\n\
    </html>\n\
    '
}

exports.ajax_setup = function() {

  $.get("/", {}, function(txt){
    var html = Xiki.children_to_html(txt);

    $("div[data-role=content]").html(html).trigger("create");
  });


  $(document).bind("pagebeforechange", function(e, data) {Xiki.navigate_ajax(e, data)});
}

exports.navigate_ajax = function(e, data) {

  if(typeof data.toPage != "string") return;

  var hash_orig = data.toPage.replace(/.+?(#|$)/, '');
  var hash = hash_orig.replace(/,/g, '/');

  $.get("/"+hash, {}, function(txt){
    Xiki.navigate(e, data, txt)
  });
}

exports.navigate = function(e, data, children) {

  if(typeof data.toPage != "string") return;

  var hash_orig = data.toPage.replace(/.+?(#|$)/, '');
  var hash = hash_orig.replace(/,/g, '/');
  label = hash.replace(/.+\//, '');
  label = label.replace(/\b\w/g, function(m){return m.toUpperCase()});

  var options = data.options;

  if(hash_orig == "home"){
    hash_orig = hash = "";
    label = "Menu";
    options.reverse = true;
  }

  var html = Xiki.children_to_html(children, hash_orig);

  // TODO: remove page if back button?

  options.dataUrl = "#"+hash;

  // Add new page to the dom!

  var page_html = '<div id="'+hash+'" data-role="page">\
  '+exports.header_html(label)+'\n\
  <div data-role="content">'+html+'</div></div>';

  $("body").append(page_html);
  var page = $("div[data-role=page]:last");

  $.mobile.changePage(page, options);

  e.preventDefault();
}

exports.children_to_html = function(txt, parent) {

  var html = "";

  // If it starts with bullet, treat them all as li
  if(txt.match(/^[+-] /)){
    html += '<ul data-role="listview" data-inset="true">';

    $.each(txt.split("\n"), function(i, item){
      if(! item) return;
      item = item.replace(/^[+-] /, "")
      item = item.replace(/\/$/, "");
      upper = item.replace(/\b\w/g, function(m){return m.toUpperCase()});

      var href = item;
      if(parent) href = parent+","+href;

      html += '<li><a href="#'+href+'">'+upper+'</a></li>';
    })
    html += "</ul>"
  }else{
    html += '<div style="margin:20px;">'
    txt = txt.replace(/^\| ?/gm, "");
    txt = txt.replace(/^> (.+)/gm, '<div style="font-weight:bold; margin-bottom:5px; font-size:20px">$1</div>');
    txt = txt.replace(/$/gm, "<br>");
    txt = txt.replace(/div><br>$/gm, "div>");
    html += txt;
    html += "</div>"
  }

  return html;
}
