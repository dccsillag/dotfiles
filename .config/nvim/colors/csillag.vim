set background=dark
let g:colors_name="csillag"

lua package.loaded['lush_theme.csillag'] = nil

lua require('lush')(require('lush_theme.csillag'))
