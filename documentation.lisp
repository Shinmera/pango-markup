(in-package #:org.shirakumo.pango-markup)

(docs:define-docs
  (function escape-char
    "Escapes the given char if necessary and writes it to stream.

This makes the following substitutions:
  <  ::= &lt;
  >  ::= &gt;
  &  ::= &amp;
  '  ::= &#39;
Any other character is written verbatim to the stream.")
  
  (type font
    "This class represents all possible font features that can be used within a markup block.

Any font property that is not a boolean can also be NIL, in which case
the value is not changed and instead inherited from the surrounding
context.

See FAMILY
See SIZE
See STYLE
See WEIGHT
See VARIANT
See STRETCH
See FEATURES")

  (function family
    "Accesses the font family name.

The font family should be a string describing the name of the family of
font that should be used.

See FONT")
  
  (function size
    "Accesses the size of the font.

The value can either be a float describing the size in points, or one
of the following size units:

Absolute: :xx-small :x-small :small :medium :large :x-large :xx-large
Relative: :smaller :larger

See FONT")
  
  (function style
    "Accesses the font's style variant.

Can be one of the following:

  :normal :oblique :italic

See FONT")
  
  (function weight
    "Accesses the font's weight or boldness.

Can be either a numeric weight, or one of the following:

  :ultra-light :light :normal :bold :ultra-bold :heavy

See FONT")
  
  (function variant
    "Accesses the font's variant.

Can be one of the following:

  :normal :small-caps

See FONT")
  
  (function stretch
    "Accesses the font's stretch factor.

Can be one of the following:

  :ultra-condensed :extra-condensed :condensed :semi-condensed
  :normal :semi-expanded :expanded :extra-expanded :ultra-expanded

See FONT")
  
  (function features
    "Accesses the font's list of feature settings.

This should be a list of strings describing the desired OpenType
features, in the same syntax as CSS uses.

See FONT")

  (type markup
    "This class represents markup information for a region of text.

Any markup property that is not a boolean can also be NIL, in which
case the value is not changed and instead inherited from the
surrounding context.

See FONT
See FOREGROUND
See BACKGROUND
See UNDERLINE
See RISE
See STRIKETHROUGH
See FALLBACK
See LANGUAGE
See LETTER-SPACING
See GRAVITY
See TAG
See MARKUP-REGIONS")

  (function font
    "Accesses the font properties of the markup region.

This should be set to an instance of FONT.

When supplied as an initarg, the font may also be a list of initargs
to use to construct a FONT instance.

See FONT
See MARKUP")
  
  (function foreground
    "Accesses the foreground text colour of the markup region.

A colour can either be the name of a colour as a string, an encoded
RGB integer, or a list of R, G, B, and optional A elements with each
element being an integer between 0 and 255.

See MARKUP")
  
  (function background
    "Accesses the background colour of the markup region.

A colour can either be the name of a colour as a string, an encoded
RGB integer, or a list of R, G, B, and optional A elements with each
element being an integer between 0 and 255.

See MARKUP")
  
  (function underline
    "Accesses the underline options of the markup region.

If set should be either an underline mode, or a list of underline mode
and underline colour. The underline mode should be one of

  :none T :single :double :low :error

Where T is synonymous with :single

The colour can either be the name of a colour as a string, an encoded
RGB integer, or a list of R, G, B, and optional A elements with each
element being an integer between 0 and 255.

See MARKUP")
  
  (function rise
    "Accesses the vertical displacement of the markup region.

This displacement can be positive or negative, and must be in pango
units.

See MARKUP")
  
  (function strikethrough
    "Accesses the strikethrough options of the markup region.

Can be either T or a list of T and the strikethrough colour.

A colour can either be the name of a colour as a string, an encoded
RGB integer, or a list of R, G, B, and optional A elements with each
element being an integer between 0 and 255.

See MARKUP")
  
  (function fallback
    "Accesses whether fallback font glyphs should be enabled.

This defaults to true and should not need to be changed.

See MARKUP")
  
  (function language
    "Accesses the language of the markup region.

The value should be a two or three letter ISO language code.

See MARKUP")
  
  (function letter-spacing
    "Accesses the inter-letter spacing of the markup region.

The value should be a float describing the spacing in points.

See MARKUP")
  
  (function gravity
    "Accesses the alignment gravity of the markup region.

If set should be either a gravity mode, or a list of gravity mode and
gravity hint.

The gravity mode can be one of: :south :east :north :west :auto
The gravity hint can be one of: :natural :strong :line

See MARKUP")
  
  (function opening-tag
    "Returns the opening tag that describes all the markup options.

You should be able to splice this tag before the text region you wish
to mark up with the given options. The end of the region to be marked
should be followed by the CLOSING-TAG.

Note that the text between the opening-tag and the closing-tag should
be properly entity-escaped using ESCAPE-CHAR.

See ESCAPE-CHAR
See CLOSING-TAG
See MARKUP")

  (function closing-tag
    "Returns the closing tag that ends the marked up text region.

You should be able to splice this tag after the text region you wish
to mark up.

Note that the text between the opening-tag and the closing-tag should
be properly entity-escaped using ESCAPE-CHAR.

See ESCAPE-CHAR
See OPENING-TAG
See MARKUP")
  
  (function markup-regions
    "Marks up the given text using the list of markup regions.

Each entry in the region list should be of the following form:

  REGION ::= (START END . MARKUP)
  START  --- An integer describing the index in the text at which
             the markup should start applying.
  END    --- An integer describing the index up until which the markup
             should apply.
  MARKUP --- Either a list of MARKUP initargs, or a MARKUP instance.

For instance, the following invocation:

  (markup-regions \"1 22 333\"
    '((0 1 :color red)
      (5 8 :font (:family \"monospace\"))))

Produces the following result:

  <span color='red'>1</span> 22 <span font_family='monospace'>333</span>

Note that while it is allowed to nest markup regions, this function
makes no attempt at detecting and warning about invalid region
combinations such as ((1 3 ..) (2 4 ..)).

See MARKUP
See FONT"))
