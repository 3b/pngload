# pngload

A PNG (Portable Network Graphics) image format decoder.

## Overview

pngload can be used to load images in the PNG image format, both from files on disk, or streams in
memory. This library was written out of frustration with png-read, which was the only native Common
Lisp code that supports PNG.

What makes pngload different than png-read?

### Speed

pngload is optimized for speed and portability across many different Common Lisp implementation and
architecture combinations. On 64-bit SBCL it is more than 3x faster than the png-read library when
decoding a particular large 4096x4096 RGBA image:

- pngload: 0.901s
- png-read: 3.058s

New in version 2.0: To overcome some performance bottlenecks, we wrote [our
own](https://github.com/3b/3bz) decompressor, as the alternatives were too slow and not easily
optimizable.

Also, we use the [mmap](https://github.com/Shinmera/mmap) library on operating systems that support
it, with a fallback path when not supported.

### Cleaner code

pngload should be a lot more hackable, and have more of an educational value than png-read, even
after adding lots of type declarations and restructuring the code away from its original cleanliness
in favor of performance.

### Full support for all chunks

The entire concrete syntax tree is parsed, and is visible as a slot in the returned `PNG` object
when decoding an image. png-read does not support some of these. Additionally, human-readable
formats are stored outside of the parse tree in the top-level object. For instance, if a chunk
specifying gamma correction is parsed, this will be stored as a floating-point value, rather than
multiplied by 100,000 and stored as an integer. Again, the raw data is stored in the `PARSE-TREE`
slot of the returned object, should you ever need more.

### Fully conforming with the PNG specification

pngload is able to load all images in [PNGSuite](http://www.schaik.com/pngsuite/) correctly.
png-read claims that it can load them all, but they were not checked for validity.

### Stores data in a format that is expected by [opticl](https://github.com/slyrus/opticl)

opticl has supported pngload since its first release, which gives you faster PNG loading
automatically if you were already using opticl.

### Support for PNG extensions

New in version 2.0, pngload supports [additional extension chunk
types](http://ftp-osl.osuosl.org/pub/libpng/documents/pngextensions.html), such as EXIF information.

### Optionally parse metadata only

pngload can optionally parse only the metadata, skipping decoding completely, in order to quickly
retrieve information about an image.

### Optionally decode as a 1-dimensional array

Instead of decoding to a format which is compatible with opticl, pngload can now decode to a flat
1-D array. This is useful for OpenGL texture uploading and some other applications.

### Optionally flip the Y axis

pngload can optionally flip the Y axis when decoding, for when the origin is expected to be at the
bottom left instead of the top left, as with OpenGL texture rendering.

### Optionally write to foreign memory

pngload can optionally write to foreign memory using static-vectors. This is useful when needing to
efficiently pass a pointer to the image data with a foreign library, such as with OpenGL.

## Install

``` lisp
(ql:quickload :pngload)
```

## Usage

Usage is quite simple:

```lisp
(pngload:load-file #p"/path/to/file.png")
```

This will return an object which includes everything you would need to render the image data, or
query it for other useful data.

Additionally, you may load a PNG datastream from a Common Lisp stream with:

```lisp
(pngload:load-stream stream)
```

Both `LOAD-FILE` and `LOAD-STREAM` accept an optional keyword argument, which can be used to disable
the slow process of decoding the image data. This can be used to very quickly get information about
the file, including but not limited to, the dimensions, last modification date, or palette
information. Image data will be unavailable with this option, obviously. To use this fast reading
method:

```lisp
(pngload:load-file #p"/path/to/file.png" :decode nil)
```

or:

```lisp
(pngload:load-stream stream :decode nil)
```

Additionally, both `LOAD-FILE` and `LOAD-STREAM` may take the following keyword arguments:

`FLATTEN` when non-NIL, will decode the image data to a 1-dimensional array, rather than the default
method which is to be compatible with opticl.

`FLIP-Y` when non-NIL, will flip the pixels on the Y axis, for when the origin is expected to be at
the bottom/left instead of the top/left.

`STATIC-VECTOR` when non-NIL, will decode to foreign memory. It is up to the user to free memory
when they are finished with it. Alternatively, you can use `WITH-PNG-IN-STATIC-VECTOR` which will
automatically free the memory for you.

### Querying Metadata

New in version 2.0, pngload has a unified API for querying different metadata that may be stored in
a PNG datastream. The `get-metadata` method can be used to query any metadata available. It accepts
a PNG object, which is returned by `load-file` or `load-stream` as per the above, as well as a key
identifying the type of metadata you want to query. If a PNG datastream does not have the metadata
requested, NIL will be returned. The following keys are recognized:

#### `:width`
The image width in pixels. This is the same as `(width png)` and is only for convenience.

#### `:height`
The image height in pixels. This is the same as `(height png)` and is only for convenience.

#### ``:bit-depth`
The number of bits per sample. This is the same as `(bit-depth png)` and is only for convenience.

#### `:color-type`
The color type of the image. This is the same as `(color-type png)` and is only for convenience. One
of the following is returned:
- `:indexed-colour`: each pixel consists of an index into a palette.
- `:greyscale`: each pixel consists of a single sample: grey.
- `:greyscale-alpha`: each pixel consists of two samples: grey and alpha.
- `:truecolour`: each pixel consists of three samples: red, green, and blue.
- `:truecolour-alpha`: each pixel consists of four samples: red, green, blue,
  and alpha.

#### `:compression-method`
The method used to compress image data chunks. For standard PNG, this can only be `:zlib`. This will
return `:unknown` if any other compression scheme was used.

#### `:interlace-method`
The interlacing method. For standard PNG, this can be either `:null` or `:adam7`. This will return
`:unknown` if any other interlacing method was used.

#### `:filter-method`
The filtering method used to decode the image. For standard PNG, this can only be `:standard`. This
will return `:unknown` if any other filter method was used.

#### `:palette`
The palette of an `:indexed-colour` image. This will return a 2-dimensional array of `(color-count
3)` representing the red, green, and blue values of each indexed color in the palette.

#### `:white-point`
The CIE 1931 reference white point. Returns two floating point values for the X and Y values.

#### `:chromaticity-red`
The CIE 1931 primary red chromaticity. Returns two floating point values for the X and Y values.

#### `:chromaticity-green`
The CIE 1931 primary green chromaticity. Returns two floating point values for the X and Y values.

#### `:chromaticity-blue`
The CIE 1931 primary blue chromaticity. Returns two floating point values for the X and Y values.

#### `:gamma`
The gamma adjustment for the desired display output intensity. Returns a floating point value.

#### `:color-profile`
The ICC color profile of the image. Returns a octet vector to be decoded by any application wishing
to make use of this.

#### `:significant-bits`
The original number of sample significant bits. Returns multiple values; one for each color channel.

#### `:srgb-rendering-intent`
Specifies how the samples should be displayed in the sRGB color space. This can be one of the
following:
- `:perceptual`: for images preferring good adaptation to the output device gamut at the expense of
  colorimetric accuracy, such as photographs.
- `:relative-colorimetric`: for images requiring colour appearance matching (relative to the output
  device white point), such as logos.
- `:saturation`: for images preferring preservation of saturation at the expense of hue and
  lightness, such as charts and graphs.
- `:absolute-colorimetric`: for images requiring preservation of absolute colorimetry, such as
  previews of images destined for a different output device (proofs).

#### `:background-color`
The default background colour to present the image against. Returns 3 values; the red, green, and
blue values.

#### `:histogram`
The approximate usage frequency of each colour in the palette. Returns an association list mapping
each palette color as a vector of 3 components to their frequencies.

#### `:transparency`
The transparency color of the image. For `:indexed-colour` images, this returns an association list
mapping each palette color as a vector of 3 components their transparency value. For `:truecolour`
images, this returns 3 values; the red, green, and blue sample value of the transparency color. For
`:greyscale` images, this returns a single value for the transparency.

#### `:pixel-dimensions`
Specifies the aspect ratio and physical size of image pixels. Returns a property list specifying the
X and Y dimensions of each pixel, as well as a unit. `:unit` can be one of `:meter` or `:unknown`.
If `:unit` is `:unknown`, physical size of the pixels is not stored, and only the aspect ratio is
present.

#### `:suggested-palettes`
Some PNG images store a "suggested palette" chunk, which stores multiple palettes, histograms, and
transparency information together. This returns an association list mapping a palette name to a
property list of suggested palette information.

#### `:last-modified`
The timestamp the image was last modified. Returns an integer in the universal time format.

#### `:text`
Arbitrary textual metadata stored in the PNG datastream. Returns a list of property lists.

## License

Copyright © 2017-2018 Michael Fiano <mail@mfiano.net>.

Licensed under the MIT License.
