if exists("b:current_syntax")
	finish
endif

syntax match offlineimaprcString " *.\+$"
syntax match offlineimaprcInteger " *[0-9]\+$"
syntax match offlineimaprcBoolean " *\(yes\|no\) *$"
syntax match offlineimaprcOperator "\v\="
syntax match offlineimaprcUnknownKey "^[^ ]\+"
syntax match offlineimaprcKey "^\(localrepository\|remoterepository\|type\|accounts\|pythonfile\|localfolders\|nametrans\|ssl\|cert_fingerprint\|remotepass\|remoteuser\|remotehost\|remoteport\|createfolders\|folderfilter\|nametrans\) *"
syntax match offlineimaprcSection "^\[.*\]$"
syntax match offlineimaprcComment "#.*$"

highlight link offlineimaprcKey Label
highlight link offlineimaprcUnknownKey Error
highlight link offlineimaprcComment Comment
highlight link offlineimaprcSection Define
highlight link offlineimaprcString String
highlight link offlineimaprcInteger Number
highlight link offlineimaprcBoolean Boolean
highlight link offlineimaprcOperator Operator

let b:current_syntax = "offlineimaprc"
