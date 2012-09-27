" Xiki with vim
"
" Requires:
"   Need to set XIKI_DIR in your environment to use
"
" Configuration options:
" g:XikiUseDefaultMapping - set to 0 to disable the default XikiMappings
"
" Commands:
" XikiLaunch --> runs the given line, pipes it to xiki at the command line and
" then puts the result in the window.

" Only load this once (no need to reload otherwise)
" (be sure to comment these next three lines out if you're
" changing the plugin)
if exists('g:XikiLoaded')
    finish
endif

function! XikiLaunch()
  ruby << EOF
    xiki_dir = ENV['XIKI_DIR']  || `xiki directory`.strip! unless defined? xiki_dir
    ['ol', 'vim/line', 'vim/tree'].each {|o| require "#{xiki_dir}/lib/xiki/#{o}"}
    line = Line.value
    indent = line[/^ +/]
    command = "xiki #{line}"
    result = `#{command}`
    Tree << result
EOF
endfunction

" Don't set mappings if user sets g:XikiUseDefaultMapping to 0
if !exists('g:XikiUseDefaultMapping') || g:XikiUseDefaultMapping
    nmap <silent> <2-LeftMouse> :call XikiLaunch()<CR>
    imap <silent> <2-LeftMouse> <C-c>:call XikiLaunch()<CR>i
    imap <silent> <C-CR> <C-c>:call XikiLaunch()<CR>i
    nmap <silent> <C-CR> :call XikiLaunch()<CR>
endif

command! XikiLaunch call XikiLaunch()
let g:XikiLoaded = 1
