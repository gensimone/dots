set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "custom"


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                      NO GUI                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""
" SYNTAX HIGHLIGHTING
""""""""""""""""""""""""
hi Constant         cterm=NONE              ctermfg=darkgreen       ctermbg=NONE
hi String           cterm=NONE              ctermfg=blue            ctermbg=NONE
hi Character        cterm=NONE              ctermfg=brown           ctermbg=NONE
hi Number           cterm=NONE              ctermfg=darkgrey        ctermbg=NONE
hi Boolean          cterm=NONE              ctermfg=darkgrey        ctermbg=NONE
hi Float            cterm=NONE              ctermfg=darkgrey        ctermbg=NONE
hi Comment          cterm=NONE              ctermfg=darkgrey        ctermbg=NONE

hi Identifier       cterm=NONE              ctermfg=darkred         ctermbg=NONE
hi Function         cterm=NONE              ctermfg=grey            ctermbg=NONE

hi Statement        cterm=NONE              ctermfg=green           ctermbg=NONE
hi Conditional      cterm=NONE              ctermfg=green           ctermbg=NONE
hi Repeat           cterm=NONE              ctermfg=yellow          ctermbg=NONE
hi Label            cterm=NONE              ctermfg=yellow          ctermbg=NONE
hi Operator         cterm=NONE              ctermfg=grey            ctermbg=NONE
hi Keyword          cterm=NONE              ctermfg=yellow          ctermbg=NONE
hi Exception        cterm=NONE              ctermfg=darkmagenta     ctermbg=NONE

hi PreProc          cterm=NONE              ctermfg=darkmagenta     ctermbg=NONE
hi Include          cterm=NONE              ctermfg=darkmagenta     ctermbg=NONE
hi Define           cterm=NONE              ctermfg=darkmagenta     ctermbg=NONE
hi Macro            cterm=NONE              ctermfg=darkmagenta     ctermbg=NONE
hi PreCondit        cterm=NONE              ctermfg=darkmagenta     ctermbg=NONE

hi Type             cterm=NONE              ctermfg=darkgreen       ctermbg=NONE
hi StorageClass     cterm=NONE              ctermfg=darkgreen       ctermbg=NONE
hi Structure        cterm=NONE              ctermfg=darkgreen       ctermbg=NONE
hi Typedef          cterm=NONE              ctermfg=darkgreen       ctermbg=NONE

hi Special          cterm=NONE              ctermfg=darkred         ctermbg=NONE
hi SpecialChar      cterm=NONE              ctermfg=darkred         ctermbg=NONE
hi Tag              cterm=NONE              ctermfg=darkred         ctermbg=NONE
hi Delimiter        cterm=NONE              ctermfg=darkred         ctermbg=NONE
hi SpecialComment   cterm=NONE              ctermfg=darkred         ctermbg=NONE
hi Debug            cterm=NONE              ctermfg=darkred         ctermbg=NONE

hi Underlined       cterm=underline         ctermfg=grey            ctermbg=NONE

hi Ignore           cterm=NONE              ctermfg=darkgrey        ctermbg=NONE

hi Error            cterm=NONE              ctermfg=white           ctermbg=darkred

hi Todo             cterm=NONE              ctermfg=black            ctermbg=darkgreen


""""""
" INTERFACE
""""""""""""""""""""""""
hi Cursor           cterm=NONE              ctermfg=black           ctermbg=brown
hi Directory        cterm=NONE              ctermfg=darkgreen       ctermbg=NONE
hi ErrorMsg         cterm=NONE              ctermfg=white           ctermbg=darkred
hi LineNr           cterm=NONE              ctermfg=darkgrey        ctermbg=NONE
hi MatchParen       cterm=NONE              ctermfg=black           ctermbg=green
hi ModeMsg          cterm=NONE              ctermfg=NONE            ctermbg=NONE
hi MoreMsg          cterm=NONE              ctermfg=darkgreen       ctermbg=NONE
hi NonText          cterm=NONE              ctermfg=darkblue        ctermbg=NONE
hi Question         cterm=NONE              ctermfg=green           ctermbg=NONE
hi SpecialKey       cterm=NONE              ctermfg=darkgreen       ctermbg=NONE
hi Title            cterm=NONE              ctermfg=darkmagenta     ctermbg=NONE
hi VertSplit        cterm=reverse           ctermfg=darkgrey        ctermbg=black
hi WarningMsg       cterm=NONE              ctermfg=white           ctermbg=darkred
hi WildMenu         cterm=NONE              ctermfg=black           ctermbg=white


""""""
" TABS
""""""""""""""""""""""""
hi TabLine          cterm=reverse           ctermfg=darkgrey        ctermbg=black
hi TabLineFill      cterm=reverse           ctermfg=darkgrey        ctermbg=grey
hi TabLineSel       cterm=reverse           ctermfg=brown           ctermbg=black


""""""
" COMPLETION
""""""""""""""""""""""""
hi Pmenu            cterm=reverse           ctermfg=darkgrey        ctermbg=black
hi PmenuSel         cterm=NONE              ctermfg=black           ctermbg=brown


""""""
" STATUS
""""""""""""""""""""""""
hi StatusLine       cterm=reverse          ctermfg=darkgray         ctermbg=black
hi StatusLineNC     cterm=reverse          ctermfg=darkgrey         ctermbg=black


""""""
" DIFF
""""""""""""""""""""""""
hi DiffAdd          cterm=NONE              ctermfg=NONE            ctermbg=darkblue
hi DiffChange       cterm=NONE              ctermfg=NONE            ctermbg=darkmagenta
hi DiffDelete       cterm=NONE              ctermfg=darkblue        ctermbg=darkcyan
hi DiffText         cterm=NONE              ctermbg=darkred         ctermbg=NONE


""""""
" VISUAL
""""""""""""""""""""""""
hi Visual           cterm=reverse           ctermfg=NONE            ctermbg=NONE
hi VisualNOS        cterm=underline         ctermfg=NONE            ctermbg=NONE


""""""
" FOLDING
""""""""""""""""""""""""
hi Folded           cterm=NONE              ctermfg=darkgrey        ctermbg=NONE
hi FoldColumn       cterm=NONE              ctermfg=darkgrey        ctermbg=NONE


""""""
" SEARCH
""""""""""""""""""""""""
hi IncSearch        cterm=NONE              ctermfg=black           ctermbg=green
hi Search           cterm=NONE              ctermfg=black           ctermbg=green


"vim: sw=4
