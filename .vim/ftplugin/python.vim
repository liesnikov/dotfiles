setlocal tabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal autoindent
setlocal smarttab
setlocal formatoptions=croql

highlight ColorColumn ctermbg=fg ctermfg=bg
call matchadd('ColorColumn', '\%81v', 100)
