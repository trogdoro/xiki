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
