(TeX-add-style-hook
 "ifedyn08"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "letter" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper" "right=1.25in" "left=1.25in" "top=1in" "bottom=1in") ("natbib" "longnamesfirst" "sort") ("fontenc" "T1") ("inputenc" "ansinew") ("caption" "margin=20pt" "aboveskip=5pt" "font=small" "labelfont=bf") ("pdfpages" "final") ("helvet" "scaled=.90") ("todonotes" "colorinlistoftodos" "textsize=small")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "geometry"
    "natbib"
    "ae"
    "fontenc"
    "inputenc"
    "amsmath"
    "amssymb"
    "url"
    "lscape"
    "setspace"
    "rotating"
    "caption"
    "sectsty"
    "pdfpages"
    "mathptmx"
    "helvet"
    "courier"
    "color"
    "arydshln"
    "todonotes")
   (TeX-add-symbols
    '("gr" 1)
    '("e" 1)
    '("smfrac" 2)
    "mc")
   (LaTeX-add-labels
    "S:technicalIssues"
    "F:all+div"
    "F:spaghettis"
    "F:spaghettis2"
    "S:hypotheses"
    "F:overlap"
    "F:overlap2"
    "F:agendaControl"
    "T:tests"
    "t:WiAndBtw"
    "S:conclusion")
   (LaTeX-add-environments
    "changemargin")
   (LaTeX-add-bibliographies
    "/home/eric/Dropbox/mydocs/magar"))
 :latex)

