## About Pango-Markup
This is a small library to handle marking up text in the [Pango-style markup](https://developer.gnome.org/pango/stable/pango-Markup.html) for you.

## How To
Marking up text is fairly easy. All you need to do is invoke `markup-text` with a list of regions and markup descriptions:

    (pango-markup:markup-regions "1 22 333"
      '((0 1 :color red)
        (5 8 :font (:family "monospace"))))
    ; => "<span color='red'>1</span> 22 <span font_family='monospace'>333</span>"

It's also possible to nest markup tags:

    (pango-markup:markup-regions "12321"
      '((0 5 :underline T)
        (1 4 :strike T)))
    ; => "<span underline='single'>1<span strikethrough='true'>232</span>1</span>"

See the `markup` and `font` class descriptions and their accessors for a list of possible options.
