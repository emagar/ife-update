(TeX-add-style-hook "ifedyn06"
 (lambda ()
    (LaTeX-add-bibliographies
     "/home/eric/Dropbox/mydocs/magar")
    (LaTeX-add-environments
     "changemargin")
    (LaTeX-add-labels
     "F:all+div"
     "F:spaghettis"
     "F:overlap"
     "F:agendaControl"
     "t:WiAndBtw"
     "T:tests")
    (TeX-add-symbols
     '("e" 1)
     '("smfrac" 2)
     "mc")
    (TeX-run-style-hooks
     "arydshln"
     "color"
     "tikz"
     "rotating"
     "setspace"
     "lscape"
     "url"
     "amssymb"
     "amsmath"
     "inputenc"
     "ansinew"
     "fontenc"
     "T1"
     "ae"
     "natbib"
     "longnamesfirst"
     "sort"
     "geometry"
     "letterpaper"
     "latex2e"
     "art12"
     "article"
     "letter"
     "12pt")))

