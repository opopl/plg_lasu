
function! lasu#fold_sections(lst, endpat)

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
		call lasu#fold_sections(strpart(a:lst,i+1), endpat)
	endif

	let endpat = '^\s*\\appendix\W\|' . endpat
	call lasu#add_syntax_fold_item(s, endpat, 0, -1)

endfun
 
function! lasu#help(topic)
endf	

function!  lasu#make_tex_folds (force)
 
 if exists('g:LASU_Folding') && !g:LASU_Folding
    return
 endif

 if &ft != 'tex'
    return
 end

  " Setup folded items lists g:LASU_Foldedxxxx
  "   1. Use default value if g:LASU_Foldedxxxxxx is not defined
  "   2. prepend default value to g:LASU_Foldedxxxxxx if it starts with ','
  "   3. append default value to g:LASU_Foldedxxxxxx if it ends with ','
  "

  let foldmisc = base#var('lasu_foldmisc')
  let foldenv  = base#var('lasu_foldenv')
  let foldcoms = base#var('lasu_foldcoms')

  " By default do not fold any commands. It looks like trying to fold
  " commands is a difficult problem since commands can be arbitrarily nested
  " and the end patterns are not unique unlike the case of environments.
  " For this to work well, we need a regexp which will match a line only if
  " a command begins on that line but does not end on that line. This
  " requires a regexp which will match unbalanced curly braces and that is
  " apparently not doable with regexps.

  " the order in which these calls are made decides the nestedness. in
  " latex, a table environment will always be embedded in either an item or
  " a section etc. not the other way around. so we first fold up all the
  " tables. and then proceed with the other regions.

  let b:numFoldItems = 0

  " ========================================================================
  " How to add new folding items                                           {{{
  " ========================================================================
  "
  " Each of the following function calls defines a syntax fold region. Each
  " definition consists of a call to the lasu#add_syntax_fold_item() function.
  " 
  " The order in which the folds are defined is important. Juggling the
  " order of the function calls will create havoc with folding. The
  " "deepest" folding item needs to be called first. For example, if
  " the \begin{table} environment is a subset (or lies within) the \section
  " environment, then add the definition for the \table first.
  "
  " The lasu#add_syntax_fold_item() function takes either 4 or 6 arguments. When it
  " is called with 4 arguments, it is equivalent to calling it with 6
  " arguments with the last two left blank (i.e as empty strings)
  "
  " The explanation for each argument is as follows:
  "    startpat: a line matching this pattern defines the beginning of a fold.
  "    endpat  : a line matching this pattern defines the end of a fold.
  "    startoff: this is the offset from the starting line at which folding will
  "              actually start
  "    endoff  : like startoff, but gives the offset of the actual fold end from
  "              the line satisfying endpat.
  "              startoff and endoff are necessary when the folding region does
  "              not have a specific end pattern corresponding to a start
  "              pattern. for example in latex,
  "              \begin{section}
  "              defines the beginning of a section, but its not necessary to
  "              have a corresponding
  "              \end{section}
  "              the section is assumed to end 1 line _before_ another section
  "              starts.
  "    startskip: a pattern which defines the beginning of a "skipped" region.
  "
  "               For example, suppose we define a \itemize fold as follows:
  "               startpat =  '^\s*\\item',
  "               endpat = '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
  "               startoff = 0,
  "               endoff = -1
  "
  "               This defines a fold which starts with a line beginning with an
  "               \item and ending one line before a line beginning with an
  "               \item or \end{enumerate} etc.
  "
  "               Then, as long as \item's are not nested things are fine.
  "               However, once items begin to nest, the fold started by one
  "               \item can end because of an \item in an \itemize
  "               environment within this \item. i.e, the following can happen:
  "
  "               \begin{itemize}
  "               \item Some text <------- fold will start here
  "                     This item will contain a nested item
  "                     \begin{itemize} <----- fold will end here because next line contains \item...
  "                     \item Hello
  "                     \end{itemize} <----- ... instead of here.
  "               \item Next item of the parent itemize
  "               \end{itemize}
  "
  "               Therefore, in order to completely define a folding item which
  "               allows nesting, we need to also define a "skip" pattern.
  "               startskip and end skip do that.
  "               Leave '' when there is no nesting.
  "    endskip: the pattern which defines the end of the "skip" pattern for
  "             nested folds.
  "
  "    Example: 
  "    1. A syntax fold region for a latex section is
  "           startpat = "\\section{"
  "           endpat   = "\\section{"
  "           startoff = 0
  "           endoff   = -1
  "           startskip = ''
  "           endskip = ''
  "    Note that the start and end patterns are thus the same and endoff has a
  "    negative value to capture the effect of a section ending one line before
  "    the next starts.
  "    2. A syntax fold region for the \itemize environment is:
  "           startpat = '^\s*\\item',
  "           endpat = '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
  "           startoff = 0,
  "           endoff = -1,
  "           startskip = '^\s*\\begin{\(enumerate\|itemize\|description\)}',
  "           endskip = '^\s*\\end{\(enumerate\|itemize\|description\)}'
  "     Note the use of startskip and endskip to allow nesting.
  "
  "
  "                                                                        }}}
  " ========================================================================
  " {{{ comment lines
"""LASU_FoldedMisc
"""_fold_comments
  if foldmisc =~ '\<comments\>'
    call lasu#add_syntax_fold_item (
      \ '^%\([^%]\|[^f]\|[^a]\|[^k]\|[^e]\)',
      \ '^[^%]',
      \ 0,
      \ -1 
      \ )
  endif
  "                                                                        }}}

  " {{{ items
"""_fold_item
  if foldmisc =~ '\<item\>'
    call lasu#add_syntax_fold_item (
      \ '^\s*\\item',
      \ '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
      \ 0,
      \ -1,
      \ '^\s*\\begin{\(enumerate\|itemize\|description\)}',
      \ '^\s*\\end{\(enumerate\|itemize\|description\)}'
      \ )
  endif
"""_fold_hitm
  if foldmisc =~ '\<hitm\>'
    call lasu#add_syntax_fold_item (
      \ '^\s*\\hitm',
      \ '^\s*\\hitm\|^\s*\\end{\(enumerate\|itemize\|description\)}',
      \ 0,
      \ -1,
      \ '^\s*\\begin{\(enumerate\|itemize\|description\)}',
      \ '^\s*\\end{\(enumerate\|itemize\|description\)}'
      \ )
  endif
"""_fold_indexitem
  if foldmisc =~ '\<indexitem\>'
    call lasu#add_syntax_fold_item (
      \ '^\s*\\indexitem',
      \ '^\s*\\indexitem\|^\s*\\end{\(enumerate\|itemize\|description\)}',
      \ 0,
      \ -1,
      \ '^\s*\\begin{\(enumerate\|itemize\|description\)}',
      \ '^\s*\\end{\(enumerate\|itemize\|description\)}'
      \ )
  endif

  "                                                                        }}}
  "if foldmisc =~ '\<hitm\>'
    "call lasu#add_syntax_fold_item (
      "\ '^\s*\\hitm',
      "\ '^\s*\\hitm\|^\s*\\end{\(enumerate\|itemize\|description\)}',
      "\ 0,
      "\ -1,
      "\ '^\s*\\begin{\(enumerate\|itemize\|description\)}',
      "\ '^\s*\\end{\(enumerate\|itemize\|description\)}'
      "\ )
  "endif


  " {{{ title
"""_fold_title
  if foldmisc =~ '\<title\>'
    call lasu#add_syntax_fold_item (
      \ '^\s*\\title\W',
      \ '^\s*\\maketitle',
      \ 0,
      \ 0
      \ )
  endif
  "                                                                        }}}
 
  " Commands and Environments                                              {{{
  " Fold the commands and environments in 2 passes.
  let pass = 0
  while pass < 2
    if pass == 0
      let lst = g:LASU_FoldedCommands
    else
      let lst = g:LASU_FoldedEnvironments
    endif
    while lst != ''
      let i = match(lst, ',')
      if i > 0
        let s = strpart(lst, 0, i)
        let lst = strpart(lst, i+1)
      else
        let s = lst
        let lst = ''
      endif
      if s != ''
        if pass == 0
          " NOTE: This pattern ensures that a command which is
          " terminated on the same line will not start a fold.
          " However, it will also refuse to fold certain commands
          " which have not terminated. eg:
          "   \commandname{something \bf{text} and 
          " will _not_ start a fold.
          " In other words, the pattern is safe, but not exact.
          call lasu#add_syntax_fold_item('^\s*\\'.s.'{[^{}]*$','^[^}]*}',0,0)
        else
          call lasu#add_syntax_fold_item('^\s*\\begin{'.s,'\(^\|\s\)\s*\\end{'.s,0,0)
        endif
      endif
    endwhile
    let pass = pass + 1
  endwhile
  "                                                                        }}}

  " Sections                                                               {{{
  if g:LASU_FoldedSections != '' 
    let prefix='^\s*\\'
    LFUN F_ReadDatFile

    call LASU_FoldSections(g:LASU_FoldedSections,
      \ prefix . join(g:LASU_FoldedSectionEnds, '\|' . prefix )
      \ )

  endif
  "                                                                        }}}
  
  " {{{ slide
"""_fold_slide
  if foldmisc =~ '\<slide\>'
    call lasu#add_syntax_fold_item (
      \ '^\s*\\begin{slide',
      \ '^\s*\\appendix\W\|^\s*\\chapter\W\|^\s*\\end{slide\|^\s*\\end{document',
      \ 0,
      \ 0
      \ )
  endif
  "                                                                        }}}

  " {{{ preamble
"""_fold_preamble
  if foldmisc =~ '\<preamble\>'
    call lasu#add_syntax_fold_item (
      \ '^\s*\\document\(class\|style\).*{',
      \ '^\s*\\begin{document}',
      \ 0,
      \ -1 
      \ )
  endif
  "                                                                        }}}

  " Manually folded regions                                                {{{
  if foldmisc =~ '\(^\|,\)<<<\(,\|$\)'
    call lasu#add_syntax_fold_item (
      \ '<<<',
      \ '>>>',
      \ 0,
      \ 0
      \ )
  endif
  
  call lasu#make_syntax_folds(a:force)
  normal! zv
 
endfunction

function! lasu#datadir ()
	let datadir = base#qw#catpath('plg','lasu data')
	return datadir
	
endfunction

function! lasu#init ()
	call lasu#initvarsfromdat()
	
endfunction

function! lasu#varsetfromdat (...)
	let varname = a:1

	let type = "List"
	if a:0 == 2
		let type = a:2
	endif

	let datafile = base#qw#catpath('plg')

	if !filereadable(datafile)
		call base#warn({ 
			\	"text": 'NO datafile for: ' . varname 
			\	})
		return 0
	endif

	let data = base#readdatfile({ 
		\   "file" : datafile ,
		\   "type" : type ,
		\	})

	call base#var(varname,data)
	return 1
endfunction

function! lasu#datfiles ()
	return base#var('lasu_datfiles')
endfunction

function! lasu#initvarsfromdat ()

	let datfiles = {}
	let datlist  = []

	let mp = { "list" : "List", "dict" : "Dictionary" }
	for type in base#qw("list dict")
		let dir = base#file#catfile([ lasu#datadir() , type ])
		if ! isdirectory(dir)
			continue
		endif
		let vars= base#find({ 
			\	"dirs" : [ dir ], 
			\	"exts" : [ "i.dat" ], 	
			\	"subdirs" : 1, 
			\	"relpath" : 1,
	   		\	"rmext"   : 1, })
		let tp = mp[type]
		for v in vars
			let d = []

			let dfs= base#find({ 
				\	"dirs" : [ dir ], 
				\	"exts" : [ "i.dat" ], 	
				\	"subdirs" : 1, 
				\	"pat"     : v, })

			let df=get(dfs,0,'')

			call add(datlist,v)
			call extend(datfiles,{ v : df }) 

			let data = base#readdatfile({ 
				\   "file" : df ,
				\   "type" : type ,
				\	})
			call base#var('lasu_'.v,data)

		endfor
	endfor

	call base#var('lasu_datlist',datlist)
	call base#var('lasu_datfiles',datfiles)
	
endfunction

" Function: AddSyntaxFoldItem (start, end, startoff, endoff [, skipStart, skipEnd]) {{{

function! lasu#add_syntax_fold_item(start, end, startoff, endoff, ...)
	if a:0 > 0
		let skipStart = a:1
		let skipEnd = a:2
	else
		let skipStart = ''
		let skipEnd = ''
	end
	if !exists('b:numFoldItems')
		let b:numFoldItems = 0
	end
	let b:numFoldItems = b:numFoldItems + 1

	exe 'let b:startPat_'.b:numFoldItems.' = a:start'
	exe 'let b:endPat_'.b:numFoldItems.' = a:end'
	exe 'let b:startOff_'.b:numFoldItems.' = a:startoff'
	exe 'let b:endOff_'.b:numFoldItems.' = a:endoff'
	exe 'let b:skipStartPat_'.b:numFoldItems.' = skipStart'
	exe 'let b:skipEndPat_'.b:numFoldItems.' = skipEnd'
endfunction 

" Function: MakeSyntaxFolds (force) {{{
" Description: This function calls FoldRegions() several times with the
"     parameters specifying various regions resulting in a nested fold
"     structure for the file.
function! lasu#make_syntax_folds(force, ...)
	if exists('b:doneFolding') && a:force == 0
		return
	end

	let skipEndPattern = ''
	if a:0 > 0
		let line1 = a:1
		let skipEndPattern = '\|'.a:2
	else
		let line1 = 1
		let r = line('.')
		let c = virtcol('.')
		
		setlocal fdm=manual
		normal! zE
	end
	if !exists('b:numFoldItems')
		b:numFoldItems = 1000000
	end
	
	let i = 1

	let maxline = line('.')

	while exists('b:startPat_'.i) && i <= b:numFoldItems
		exe 'let startPat = b:startPat_'.i
		exe 'let endPat = b:endPat_'.i
		exe 'let startOff = b:startOff_'.i
		exe 'let endOff = b:endOff_'.i
		
		let skipStart = ''
		let skipEnd = ''
		if exists('b:skipStartPat_'.i)
			exe 'let skipStart = b:skipStartPat_'.i
			exe 'let skipEnd = b:skipEndPat_'.i
		end
		exe line1
		let lastLoc = line1

		if skipStart != ''
			call lasu#init_stack('BeginSkipArray')
			call lasu#fold_regions_with_skip(startPat, endPat, startOff, endOff, skipStart, skipEnd, 1, line('$'))
			" call PrintError('done folding ['.startPat.']')
		else
			call lasu#fold_regions_with_noskip(startPat, endPat, startOff, endOff, 1, line('$'), '')
		end

		let i = i + 1
	endwhile

	exe maxline
	
	if a:0 == 0
		exe r
		exe "normal! ".c."|"
		if foldlevel(r) > 1
			exe "normal! ".(foldlevel(r) - 1)."zo"
		end
		let b:doneFolding = 0
	end
endfunction

" ==============================================================================
"        File: syntaxFolds.vim
"      Author: Srinath Avadhanula
"              ( srinath@fastmail.fm )
" Last Change: Sun Oct 27 01:00 AM 2002 PST
" Description: Emulation of the syntax folding capability of vim using manual
"              folding
"
" This script provides an emulation of the syntax folding of vim using manual
" folding. Just as in syntax folding, the folds are defined by regions. Each
" region is specified by a call to FoldRegions() which accepts 4 parameters:
"
"    call FoldRegions(startpat, endpat, startoff, endoff)
"
"    startpat: a line matching this pattern defines the beginning of a fold.
"    endpat  : a line matching this pattern defines the end of a fold.
"    startoff: this is the offset from the starting line at which folding will
"              actually start
"    endoff  : like startoff, but gives the offset of the actual fold end from
"              the line satisfying endpat.
"              startoff and endoff are necessary when the folding region does
"              not have a specific end pattern corresponding to a start
"              pattern. for example in latex,
"              \begin{section}
"              defines the beginning of a section, but its not necessary to
"              have a corresponding
"              \end{section}
"              the section is assumed to end 1 line _before_ another section
"              starts.
"    startskip: a pattern which defines the beginning of a "skipped" region.
"
"               For example, suppose we define a \itemize fold as follows:
"               startpat =  '^\s*\\item',
"               endpat = '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
"               startoff = 0,
"               endoff = -1
"
"               This defines a fold which starts with a line beginning with an
"               \item and ending one line before a line beginning with an
"               \item or \end{enumerate} etc.
"
"               Then, as long as \item's are not nested things are fine.
"               However, once items begin to nest, the fold started by one
"               \item can end because of an \item in an \itemize
"               environment within this \item. i.e, the following can happen:
"
"               \begin{itemize}
"               \item Some text <------- fold will start here
"                     This item will contain a nested item
"                     \begin{itemize} <----- fold will end here because next line contains \item...
"                     \item Hello
"                     \end{itemize} <----- ... instead of here.
"               \item Next item of the parent itemize
"               \end{itemize}
"
"               Therefore, in order to completely define a folding item which
"               allows nesting, we need to also define a "skip" pattern.
"               startskip and end skip do that.
"               Leave '' when there is no nesting.
"    endskip: the pattern which defines the end of the "skip" pattern for
"             nested folds.
"
"    Example: 
"    1. A syntax fold region for a latex section is
"           startpat = "\\section{"
"           endpat   = "\\section{"
"           startoff = 0
"           endoff   = -1
"           startskip = ''
"           endskip = ''
"    Note that the start and end patterns are thus the same and endoff has a
"    negative value to capture the effect of a section ending one line before
"    the next starts.
"    2. A syntax fold region for the \itemize environment is:
"           startpat = '^\s*\\item',
"           endpat = '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
"           startoff = 0,
"           endoff = -1,
"           startskip = '^\s*\\begin{\(enumerate\|itemize\|description\)}',
"           endskip = '^\s*\\end{\(enumerate\|itemize\|description\)}'
"     Note the use of startskip and endskip to allow nesting.
"
"
" Each time a call is made to FoldRegions(), all the regions (which might be
" disjoint, but not nested) are folded up.
" Nested folds can be created by successive calls to FoldRegions(). The first
" call defines the region which is deepest in the folding. See MakeTexFolds()
" for an idea of how this works for latex files.



" }}}


" }}}
" lasu#fold_regions_with_skip: folding things such as \item's which can be nested. {{{
function! lasu#fold_regions_with_skip(startpat, endpat, startoff, endoff, startskip, endskip, line1, line2)
	exe a:line1
	" count the regions which have been skipped as we go along. do not want to
	" create a fold which with a beginning or end line in one of the skipped
	" regions.
	let skippedRegions = ''

	" start searching for either the starting pattern or the end pattern.
	while search(a:startskip.'\|'.a:endskip, 'W')
	
		if getline('.') =~ a:endskip

			let lastBegin = lasu#pop('BeginSkipArray')
			" call PrintError('popping '.lastBegin.' from stack and folding till '.line('.'))
			call lasu#fold_regions_with_noskip(a:startpat, a:endpat, a:startoff, a:endoff, lastBegin, line('.'), skippedRegions)
			let skippedRegions = skippedRegions.lastBegin.','.line('.').'|'


		" if this is the beginning of a skip region, then, push this line as
		" the beginning of a skipped region.
		elseif getline('.') =~ a:startskip

			" call PrintError('pushing '.line('.').' ['.getline('.').'] into stack')
			call lasu#push('BeginSkipArray', line('.'))

		end
	endwhile

	" call PrintError('with skip starting at '.a:line1.' returning at line# '.line('.'))
endfunction

" }}}
" lasu#fold_regions_with_noskip: folding things such as \sections which do not nest. {{{
function! lasu#fold_regions_with_noskip(startpat, endpat, startoff, endoff, line1, line2, skippedRegions)
	exe a:line1

	" call PrintError('line1 = '.a:line1.', searching from '.line('.').'... for ['.a:startpat.'')
	let lineBegin = lasu#my_search(a:startpat, 'in')
	" call PrintError('... and finding it at '.lineBegin)

	while lineBegin <= a:line2
		if lasu#is_in_skipped_region(lineBegin, a:skippedRegions)
			let lineBegin = lasu#my_search(a:startpat, 'out')
			" call PrintError(lineBegin.' is being skipped')
			continue
		end
		let lineEnd = lasu#my_search(a:endpat, 'out')
		while lasu#is_in_skipped_region(lineEnd, a:skippedRegions) && lineEnd <= a:line2
			let lineEnd = lasu#my_search(a:endpat, 'out')
		endwhile
		if lineEnd > a:line2
			exe (lineBegin + a:startoff).','.a:line2.' fold'
			break
		else
			" call PrintError ('for ['.a:startpat.'] '.(lineBegin + a:startoff).','.(lineEnd + a:endoff).' fold')
			exe (lineBegin + a:startoff).','.(lineEnd + a:endoff).' fold'
		end

		" call PrintError('line1 = '.a:line1.', searching from '.line('.').'... for ['.a:startpat.'')
		let lineBegin = lasu#my_search(a:startpat, 'in')
		" call PrintError('... and finding it at '.lineBegin)
	endwhile

	exe a:line2
	return
endfunction

" }}}
" lasu#init_stack: initialize a stack {{{
function! lasu#init_stack(name)
	exe 'let s:'.a:name.'_numElems = 0'
endfunction
" }}}
" lasu#push: push element into stack {{{
function! lasu#push(name, elem)
	exe 'let numElems = s:'.a:name.'_numElems'
	let numElems = numElems + 1
	exe 'let s:'.a:name.'_Element_'.numElems.' = a:elem'
	exe 'let s:'.a:name.'_numElems = numElems'
endfunction
" }}}
" lasu#pop: pops element off stack {{{
function! lasu#pop(name)
	exe 'let numElems = s:'.a:name.'_numElems'
	if numElems == 0
		return ''
	else
		exe 'let ret = s:'.a:name.'_Element_'.numElems
		let numElems = numElems - 1
		exe 'let s:'.a:name.'_numElems = numElems'
		return ret
	end
endfunction
" }}}
" MySearch: just like search(), but returns large number on failure {{{
function! lasu#my_search(pat, opt)
	if a:opt == 'in'
		if getline('.') =~ a:pat
			let ret = line('.')
		else
			let ret = search(a:pat, 'W')
		end
	else
		normal! $
		let ret = search(a:pat, 'W')
	end

	if ret == 0
		let ret = line('$') + 1
	end
	return ret
endfunction
" }}}
" Function: lasu#is_in_skipped_region (lnum, regions) {{{
" Description: finds whether a given line number is within one of the regions
"              skipped.
function! lasu#is_in_skipped_region(lnum, regions)
	let i = 1
	let subset = base#strntok(a:regions, '|', i)
	while subset != ''
		let n1 = base#strntok(subset, ',', 1)
		let n2 = base#strntok(subset, ',', 2)
		if a:lnum >= n1 && a:lnum <= n2
			return 1
		end

		let subset = base#strntok(a:regions, '|', i)
		let i = i + 1
	endwhile

	return 0
endfunction " }}}

" vim600:fdm=marker

	
