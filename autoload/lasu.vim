
function! lasu#foldsections(lst, endpat)

	let i = match(a:lst, ',')

	if i > 0
		let s = strpart(a:lst, 0, i)
	else
		let s = a:lst
	endif

	if s =~ '%%fakesection'
		let s = '^\s*' . s
	else
		let s = '^\s*\\' . s . '\W'
	endif

	let endpat = s . '\|' . a:endpat

	if i > 0
		call lasu#foldsections(strpart(a:lst,i+1), endpat)
	endif

	let endpat = '^\s*\\appendix\W\|' . endpat
	call AddSyntaxFoldItem(s, endpat, 0, -1)

endfun
 
