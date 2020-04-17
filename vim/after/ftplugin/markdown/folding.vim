setlocal foldmethod=expr
setlocal foldexpr=GetMarkdownFold(v:lnum)

function! GetMarkdownFold(lnum)

	" By default level is the same as previous line
	let level = '='

	" Get syntax highlighting groups
	let hiGroups = map(synstack(a:lnum, 1), 'synIDattr(v:val, "name")')
	if (len(hiGroups) > 0)

		" Look for Markdown Header N syntax group
		for grp in hiGroups
			let m = matchlist(grp, '^markdownH\([0-9]\+\)$')
			if len(m) > 0
				" Start fold level for this header
				let level = '>'.(m[1] - 1)
				break
			endif
		endfor
	endif

	return level
endfunction
