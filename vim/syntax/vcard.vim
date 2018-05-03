" Vim syntax file
" Language: VCARD
" Maintainer: Pierrick Roger
" Latest Revision: 22 January 2016
" Based on https://tools.ietf.org/html/rfc6350

if exists("b:current_syntax")
	finish
endif
let b:current_syntax = "vcard"

""""""""""""
" KEYWORDS "
""""""""""""

syn keyword vcardBlockField BEGIN END
syn keyword vcardField EMAIL TITLE VERSION N FN ORG TEL ADR URL CATEGORIES ROLE
syn keyword vcardParam TYPE VALUE LABEL

"""""""""""
" MATCHES "
"""""""""""

syn match vcardEmail    '[a-zA-Z0-9.-]\+@[a-zA-Z0-9.-]\+'
syn match vcardTelUri   'tel:+[0-9.]\+'

"""""""""""
" REGIONS "
"""""""""""

syn region vcardBlock start="BEGIN:VCARD" end="END:VCARD" fold transparent contains=vcardParam,vcardEmail,vcardString,vcardTelUri,vcardField
syn region vcardString start='"' end='"' contained

""""""""""
" COLORS "
""""""""""

hi def link vcardBlockField  Statement
hi def link vcardField  Function
hi def link vcardParam  Type
hi def link vcardEmail  Constant
hi def link vcardTel    Constant
hi def link vcardString String
