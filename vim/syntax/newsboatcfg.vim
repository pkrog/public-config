if exists("b:current_syntax")
	finish
endif

syntax match newsboatcfgBoolean " *\(yes\|no\) *$"
syntax region newsboatcfgString start=/"/ end=/"/
syntax match newsboatcfgInteger " *[0-9]\+$"
syntax match newsboatcfgKey "^\(auto-reload\|reload-time\|color\|browser\|bind-key\)"
syntax match newsboatcfgKey2 " \(info\|background\|listnormal_unread\|listfocus_unread\|listnormal\|listfocus\|article\|LEFT\|RIGHT\) "
syntax match newsboatcfgComment "#.*$"

highlight link newsboatcfgKey Label
highlight link newsboatcfgKey2 Define
highlight link newsboatcfgComment Comment
highlight link newsboatcfgBoolean Boolean
highlight link newsboatcfgString String
highlight link newsboatcfgInteger Number

" Define coloring of color definitions
let bgcol = 0
for fgcol in range(255, 1, -1)
	exec 'syntax match newsboatcfgColorFg'.fgcol.' /color'.fgcol.' *color'.bgcol.'/'
	exec 'highlight link newsboatcfgColorFg'.fgcol.' ColorFg'.fgcol.'Bg'.bgcol
endfor

let b:current_syntax = "newsboatcfg"
