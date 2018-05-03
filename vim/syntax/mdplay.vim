source $VIMRUNTIME/syntax/markdown.vim

""""""""""""
" KEYWORDS "
""""""""""""

syntax keyword mdplayPieceTitre FMpiecetitre
syntax keyword mdplayPieceSousTitre FMpiecesoustitre
syntax keyword mdplayPieceTitreDate FMpiecetitredate
syntax keyword mdplayPieceTitreActeurs FMpiecetitreacteurs
syntax keyword mdplayListeActeurs FMnomlisteacteurs
syntax keyword mdplaySceneActeurs FMpieceentetesceneacteurs
syntax keyword mdplayRepliqueActeur FMpiecenomacteurreplique
syntax keyword mdplayAir FMpiecetitreair
syntax keyword mdplayProse FMpiecetexteprose
syntax keyword mdplayDidascalieDroite FMpiecedidascaliedroite
syntax keyword mdplayVers01 FMpiecetextevers01
syntax keyword mdplayVers02 FMpiecetextevers02
syntax keyword mdplayVers03 FMpiecetextevers03
syntax keyword mdplayVers04 FMpiecetextevers04
syntax keyword mdplayVers05 FMpiecetextevers05
syntax keyword mdplayVers06 FMpiecetextevers06
syntax keyword mdplayVers07 FMpiecetextevers07
syntax keyword mdplayVers08 FMpiecetextevers08
syntax keyword mdplayVers09 FMpiecetextevers09
syntax keyword mdplayVers10 FMpiecetextevers10
syntax keyword mdplayVers11 FMpiecetextevers11
syntax keyword mdplayVers12 FMpiecetextevers12

"""""""""""
" REGIONS "
"""""""""""

syn region mdplayStyle start="^@\[" end="]" contains=mdplayPieceTitre,mdplayPieceSousTitre,mdplayPieceTitreDate,mdplayPieceTitreActeurs,mdplayListeActeurs,mdplaySceneActeurs,mdplayRepliqueActeur,mdplayAir,mdplayProse,mdplayDidascalieDroite,mdplayVers01,mdplayVers02,mdplayVers03,mdplayVers04,mdplayVers05,mdplayVers06,mdplayVers07,mdplayVers08,mdplayVers09,mdplayVers10,mdplayVers11,mdplayVers12

""""""""""
" COLORS "
""""""""""

hi mdplayStyle              ctermfg=27
hi mdplayPieceTitre         ctermfg=5
hi mdplayPieceSousTitre     ctermfg=127
hi mdplayPieceTitreDate     ctermfg=20
hi mdplayPieceTitreActeurs  ctermfg=2
hi mdplayListeActeurs       ctermfg=28
hi mdplaySceneActeurs       ctermfg=40
hi mdplayRepliqueActeur     ctermfg=22
hi mdplayAir                ctermfg=6
hi mdplayProse              ctermfg=94
hi mdplayDidascalieDroite   ctermfg=184
hi mdplayVers01             ctermfg=1
hi mdplayVers02             ctermfg=88
hi mdplayVers03             ctermfg=124
hi mdplayVers04             ctermfg=90
hi mdplayVers05             ctermfg=11
hi mdplayVers06             ctermfg=184
hi mdplayVers07             ctermfg=178
hi mdplayVers08             ctermfg=58
hi mdplayVers09             ctermfg=70
hi mdplayVers10             ctermfg=34
hi mdplayVers11             ctermfg=33
hi mdplayVers12             ctermfg=55
