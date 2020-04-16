if exists("b:current_syntax")
	finish
endif

syntax match lynxOutputPageLink "^Page link:.*$"
syntax region lynxOutputBulletStar start=/^\s*\*/ end=/$/ contains=lynxOutputLink,lynxOutputHttpLink
syntax region lynxOutputBulletPlus start=/^\s*+/ end=/$/ contains=lynxOutputLink,lynxOutputHttpLink
syntax region lynxOutputSharp start=/^\s*\#/ end=/$/ contains=lynxOutputLink,lynxOutputHttpLink
syntax region lynxOutputLink start=!\[[a-z-]\+://!hs=s+1 end=!]!he=e-1 skip=!\[[^\]]*]!
syntax region lynxOutputHttpLink start=!\[https\?://!hs=s+1 end=!]!he=e-1 skip=!\[[^\]]*]!

highlight link lynxOutputPageLink   LynxOutputPageTitle
highlight link lynxOutputHttpLink   LynxOutputHttpLink
highlight link lynxOutputLink       LynxOutputLink
highlight link lynxOutputBulletStar LynxOutputBulletStar
highlight link lynxOutputBulletPlus LynxOutputBulletPlus
highlight link lynxOutputSharp      LynxOutputSharp

let b:current_syntax = "lynx_output"
