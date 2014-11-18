Browser.js("$.makeArray( $('script[src]').map(function(i, o){return '@'+$(o).attr('src')}) ).join('\\n')")
