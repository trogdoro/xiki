if exists("loaded_xikivim")
    finish
endif
" if v:version < 700
"     echoerr "XikiVim: this plugin requires vim >= 7!"
"     finish
" endif
let loaded_xikivim = 1

function! XikiLaunch()
  ruby << eoruby
    # $".clear # dev
    $XIKI_DIR ||= ENV['XIKI_DIR'] || %x,xiki directory,.strip

    begin
      require "#{$XIKI_DIR}/lib/xiki/vim.rb"
    rescue LoadError
      Vim.message("Couldn't find Xiki home!")
      raise
    end

    path = XikiVim::construct_path $curbuf
    XikiVim::take_action $curbuf, path
eoruby
endfunction
command! XikiLaunch :call XikiLaunch()

if !exists("xikivim_no_mappings")
    inoremap <silent> <2-LeftMouse> <C-c>:XikiLaunch<CR>a
    nnoremap <silent> <2-LeftMouse> <C-c>:XikiLaunch<CR>i
    inoremap <silent> <Leader><Leader> <C-c>:XikiLaunch<CR>a
    nnoremap <silent> <Leader><Leader> :XikiLaunch<CR>
endif

