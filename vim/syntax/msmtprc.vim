if exists("b:current_syntax")
	finish
endif

syntax keyword msmtprcKeyword account defaults
syntax match msmtprcUnknownKey "^[^ ]\+"
syntax match msmtprcKey "^\(host\|port\|logfile\|from\|auth\|tls_certcheck\|tls_fingerprint\|tls_starttls\|tls\|user\|password\)"
syntax match msmtprcComment "#.*$"
syntax match msmtprcString " .\+$"
syntax match msmtprcInteger " [0-9]\+$"
syntax match msmtprcBoolean " \(off\|on\) *$"

highlight link msmtprcKeyword Keyword
highlight link msmtprcKey Label
highlight link msmtprcUnknownKey Error
highlight link msmtprcComment Comment
highlight link msmtprcString String
highlight link msmtprcInteger Number
highlight link msmtprcBoolean Boolean

let b:current_syntax = "msmtprc"
