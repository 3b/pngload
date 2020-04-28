# pngload

A PNG (Portable Network Graphics) image format decoder.

## Overview

pngload can be used to load images in the PNG image format, both from files on
disk, or streams in memory. This library was written out of frustration with
png-read, which was the only native Common Lisp code that supports PNG.

What makes pngload different than png-read?

### Speed

pngload is optimized for speed and portability across many different Common Lisp
implementation and architecture combinations. On 64-bit SBCL it is roughly 3x
faster than the png-read library when decoding a large 4096x4096 RGBA image:

- pngload: 1.039s
- png-read: 2.986s

New in version 2.0: To overcome some performance bottlenecks, we wrote [our
own](https://github.com/3b/3bz) decompressor, as the alternatives were too slow
and not easily optimizable.

Also, we use the [mmap](https://github.com/Shinmera/mmap) library on operating
systems that support it, with a fallback path when not supported.

### Cleaner code

pngload should be a lot more hackable, and have more of an educational value
than png-read, even after adding lots of type declarations and restructuring the
code away from its original cleanliness in favor of performance.

### Full support for all chunks

The entire concrete syntax tree is parsed, and is visible as a slot in the
returned `PNG` object when decoding an image. png-read does not support some of
these. Additionally, human-readable formats are stored outside of the parse tree
in the top-level object. For instance, if a chunk specifying gamma correction is
parsed, this will be stored as a floating-point value, rather than multiplied by
100,000 and stored as an integer. Again, the raw data is stored in the
`PARSE-TREE` slot of the returned object, should you ever need more.

### Fully conforming with the PNG specification

pngload is able to load all images in
[PNGSuite](http://www.schaik.com/pngsuite/) correctly. png-read claims that it
can load them all, but they were not checked for validity.

### Stores data in a format that is expected by [opticl](https://github.com/slyrus/opticl)

opticl has supported pngload since its first release, which gives you faster PNG
loading automatically if you were already using opticl.

### Support for PNG extensions

New in version 2.0, pngload supports [additional extension chunk
types](http://ftp-osl.osuosl.org/pub/libpng/documents/pngextensions.html), such
as EXIF information.

### Optionally decode metadata only

pngload can optionally parse only the metadata, skipping decoding completely, in
order to quickly retrieve information about an image.

### Optionally decode as a 1-dimensional array

Instead of decoding to a format which is compatible with opticl, pngload can now
decode to a flat 1-D array.This is useful for OpenGL texture uploading and some
other applications.

### Optionally flip the Y axis

pngload can optionally flip the Y axis when decoding, for when the origin is
expected to be at the bottom left instead of the top left, as with OpenGL
texture rendering.

### Optionally write to foreign memory

pngload can optionally write to foreign memory using static-vectors. This is
useful when needing to efficiently pass a pointer to the image data with a
foreign library, such as with OpenGL.

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
(pngload:load-file #p"/path/to/file.png" :decode nil)
```

or:

```lisp
(pngload:load-stream stream :decode nil)
```

Additionally, both `LOAD-FILE` and `LOAD-STREAM` may take the following keyword
arguments:

`FLATTEN` when non-NIL, will decode the image data to a 1-dimensional array,
rather than the default method which is to be compatible with opticl.

`FLIP-Y` when non-NIL, will flip the pixels on the Y axis, for when the origin
is expected to be at the bottom/left instead of the top/left.

`STATIC-VECTOR` when non-NIL, will decode to foreign memory. It is up to the
user to free memory when they are finished with it. Alternatively, you can use
`WITH-PNG-IN-STATIC-VECTOR` which will automatically free the memory for you.


## License

Copyright © 2017-2018 [Michael Fiano](mailto:mail@michaelfiano.com).

Licensed under the MIT License.
