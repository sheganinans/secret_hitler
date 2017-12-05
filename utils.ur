fun fold_css (l : list css_class) : css_class = List.foldr (fn c s => classes c s) null l
