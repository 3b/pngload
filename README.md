# pngload

A PNG (Portable Network Graphics) image format decoder.

## Overview

pngload can be used to load images in the PNG image format, both from files on
disk, or streams in memory. This library was written out of frustration with
`png-read`, which is the only native Common Lisp code that supports PNG. Now,
there shall be a choice.

What makes `pngload` different than `png-read`?

- Speed

`png-read` is very slow. For a simple test on modern hardware, it takes
`png-read` 0.95 [1] seconds to load an image that takes `cl-png` (A CFFI wrapper
for libpng) 0.028s. `pngload` takes 0.145s.

[1] Note that I recently applied some SBCL-specific compiler optimizations to `png-read`, so this figure will be lower on SBCL once it is pushed in the next Quicklisp dist release. Still though, `pngload` is approximately 2.5-5x faster, depending on the image type.

- Cleaner code

`pngload` should be a lot more hackable, and have more of an educational value
than `png-read`, even after adding lots of type declarations and restructuring
the code away from its original cleanliness in favor of performance.

- Full support for all chunks.

The entire concrete syntax tree is parsed, and is
  visible as a slot in the returned `PNG-OBJECT` object when decoding an image.
  `png-read` does not support some of these. Additionally, human-readable
  formats are stored outside of the parse tree in the top-level object. For
  instance, if a chunk specifying gamma correction is parsed, this will be
  stored as a floating-point value, rather than multiplied by 100,000 and
  stored as an integer. Again, the raw data is stored in the `PARSE-TREE` slot
  of the returned object, should you ever need more.

- Fully conforming with the PNG specification

Able to load all images in
  [PNGSuite](http://www.schaik.com/pngsuite/) correctly. `png-read` claims that
  it can load them all, but they were not checked for validity.

- Stores data in a format that is expected of
  [opticl](https://github.com/slyrus/opticl).
  
  Makes transitioning to `pngload`
  easier and faster in the future. Its author has expressed interest in
  replacing or at least adding `pngload` as an optional backend.

- Metadata only

Can parse only the metadata if desired, and skip decoding, in order to quickly
  retrieve information about an image without actually decoding it.

## Install

``` lisp
(ql:quickload :pngload)
```

## Usage

Usage is quite simple:

```lisp
(pngload:load-file #p"/path/to/file.png")
```

This will return an object which includes everything you would need to render
the image data, or query it for other useful data.

Additionally, you may load a PNG datastream from a Common Lisp stream with:

```lisp
(pngload:load-stream stream)
```

Both `LOAD-FILE` and `LOAD-STREAM` accept an optional keyword argument, which
can be used to disable the slow process of decoding the image data. This can be
used to very quickly get information about the file, including but not limited
to, the dimensions, last modification date, or palette information. Image data
will be unavailable with this option, obviously. To use this fast reading
method:

```lisp
(pngload:load-file #p"/path/to/file.png" :decodep nil)
```

or:

```lisp
(pngload:load-stream stream :decodep nil)
```

## License

Copyright © 2017 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
