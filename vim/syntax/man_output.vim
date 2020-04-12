if exists("b:current_syntax")
	finish
endif

syntax match manOutputSection /\v^[A-Z][A-Z ]+/
syntax match manOutputTitle   /\v(^[A-Z0-9_-]+\([0-9n]\).*$|^.*[A-Z]+\([0-9]\)$)/
syntax match manOutputShortOpt /\v[^A-Za-z0-9_-]-[A-Za-z0-9][^A-Za-z0-9_-]/hs=s+1,he=e-1
syntax match manOutputLongOpt   /\v[^A-Za-z0-9_-]--?[A-Za-z0-9_-]+/hs=s+1
syntax match manOutputRef       /\v[a-z0-9_-]+\([0-9n]\)/

highlight link manOutputTitle    ManOutputTitle
highlight link manOutputSection  ManOutputSection
highlight link manOutputShortOpt ManOutputOption
highlight link manOutputLongOpt  ManOutputOption
highlight link manOutputRef      ManOutputReference

let b:current_syntax = "man_output"
