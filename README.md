# Incudine

## Music/DSP programming environment for Common Lisp

[Incudine](http://incudine.sourceforge.net) is useful to design software synthesizers or sound plugins
from scratch, exploiting the expressive power of Common Lisp, without
the constraint to use pre-built unit generators. It is also a
compositional tool that allows to produce high quality sounds
controllable at the sample level, defining and redefining the digital
signal processors and the musical structures on-the-fly.

Incudine introduces the Virtual UGen (VUG), a new mechanism for
defining primitive unit generators by writing concise and reusable
code. The definition of a VUG contains lisp code and/or other nested
VUGs (or "real" UGens), and it is re-arranged (i.e. after the inference
of init- and performance-time actions) and compiled efficiently during
the creation of a DSP. An ironic example in [spaghetti.cudo](doc/tutorials/spaghetti.cudo)
includes the macro expansion of a "recipe processor".

## Features

- Realtime synthesis with sample-by-sample or block-by-block processing (without a realtime garbage collector but it's simple to define cons-free DSP)
- Optional recovery of audio cycles suspended during gc by processing the cached audio inputs and MIDI events. For example, it allows HD recording without losing audio inputs during gc if there aren't xruns.
- Sample accurate callbacks
- Bounce to disk and bounce to memory buffer
- Arbitrary precision. The sample type is double float but the unit generators and the DSP's don't require a particular type. In practice the init-time and performance-time functions of UGEN or DSP are arbitrary lisp functions regulated by the context (i.e. realtime audio).
- Optional standalone executable
- Optional score (rego) files with time-tagged lisp functions, lisp statements, lisp tags and arbitrary score statements
- Graph of DSP's
- Collection of useful re-definable VUG's
- Collection of GEN routines to fill memory buffers
- MIDI support via [PortMidi](http://portmedia.sourceforge.net/portmidi) and [Jack MIDI](https://jackaudio.org/)
- MIDI File I/O support
- Open Sound Control support
- Stream socket support
- Serial I/O support
- Tempo change with arbitrary curves
- Import/Export a musical scale from/to scale file format
- MIDI tuning by sending a bulk tuning dump message with arbitrary checksum function or 128 single note tuning change messages (Exclusive Real Time)
- Frequencies of a TUNING structure changed with the data received from a MIDI bulk tuning dump message
- Flexible voicer for voice management
- The score (rego) file format supports the [Org](https://orgmode.org) markup language. It is possible to edit and organize score files with spreedsheet-like capabilities, headlines, unordered lists, blocks, properties, hyperlinks, todo items, tags, deadlines, scheduling, etc.
- It is possible to create and edit a rego file in SES (Simple Emacs Spreadsheet) mode.
- Optional interface to interact with the sound editor [Snd](https://ccrma.stanford.edu/software/snd)
- Optional cudere-clm package, the sound synthesis package CLM (Common Lisp Music) implemented in Incudine
- Optional interface to define a VUG for a [LADSPA audio plugin](https://www.ladspa.org)
- Optional interface to define a VUG for a [LV2 audio plugin](https://lv2plug.in)
- Optional [FluidSynth](https://www.fluidsynth.org) SoundFont synthesizer support
- Incudine is released under the GNU General Public License (GPL) version 2 or later

## Installation

The latest source code can be obtained via Git:

```
git clone git://git.code.sf.net/p/incudine/incudine
```

Mirror:

```
git clone git://github.com/titola/incudine.git
```

### Requirements

Incudine works with [SBCL](http://www.sbcl.org), an implementation of
ANSI Common Lisp with a high-performance native compiler.

Common Lisp packages:

- [ALEXANDRIA](https://common-lisp.net/project/alexandria)
- [BORDEAUX-THREADS](https://common-lisp.net/project/bordeaux-threads)
- [CFFI](https://common-lisp.net/project/cffi)
- [SWAP-BYTES](https://github.com/sionescu/swap-bytes)
- [Optional] [Linedit](https://common-lisp.net/project/linedit) for incudine command

Foreign libraries:

- Pthread
- [JACK](https://jackaudio.org) or [PortAudio](http://www.portaudio.com) >= 1.9
- [PortMidi](http://portmedia.sourceforge.net/portmidi)
- [libsndfile](http://www.mega-nerd.com/libsndfile) >= 1.0.19
- [FFTW](http://www.fftw.org) >= 3.0
- [GNU Scientific Library (GSL)](https://www.gnu.org/software/gsl)
- [Optional] [FluidSynth](https://www.fluidsynth.org)
- [Optional] [Lilv](http://drobilla.net/software/lilv)

Some Linux distributions provide separated devel packages necessary
to compile Incudine, for example `jack-audio-connection-kit-devel`,
`portaudio-devel`, etc.

### Configuration

Put the incudine directory where ASDF finds your systems, for example:

```
mv incudine ${HOME}/common-lisp/
```

If you use [Quicklisp](https://www.quicklisp.org):

```
mv incudine /path/to/quicklisp/local-projects/
```

Edit and copy the [sample configuration file](incudinerc-example),
with particular attention to the priorities `*RT-PRIORITY*`,
`*NRT-PRIORITY*` and `*RECEIVER-DEFAULT-PRIORITY*`

```
cp /path/to/incudine/incudinerc-example ${HOME}/.incudinerc
```

If MMCSS (Multimedia Class Scheduler Service) is available on Windows,
the realtime thread is associated with the "Pro Audio" task.

Here is a test:

```
(require :incudine)
(in-package :scratch)
(dsp! hello-world () (out (sine 440 .3)))
(rt-start)
;; You should hear an oscillation at 440 Hz.
(hello-world)
(free 0)
(rt-stop)
;; It writes a soundfile.
(bounce-to-disk ("/tmp/test.wav" :channels 1 :duration 1) (hello-world))
```

[GNU Emacs](https://www.gnu.org/software/emacs) and [Texinfo](https://www.gnu.org/software/texinfo)
are required to build the documentation:

```
cd doc/manual && make info html pdf
```

This builds the Info, HTML and PDF documentation from the Org and
Texinfo sources.

If you want to create and install the `incudine` command:

```
cd src && sh install_executable
```

The options for the script `install_executable` are:

```
--prefix=PREFIX       install architecture-independent files in PREFIX
                      [/usr/local]
--bindir=DIR          user executables [PREFIX/bin]
--swank-loader=PATH   support for Swank server with path to swank-loader.lisp
--without-aclrepl     do not use Allegro CL-style Read-Eval-Print Loop.
--with-clm            use cudere-clm, the Incudine version of CLM
--with-linedit        support for Linedit, readline-style library in CL.
--with-fluidsynth     support for FluidSynth SoundFont synthesizer.
--with-ladspa         support for LADSPA plugins.
--with-lv2            support for LV2 plugins.
--with-snd            support for the sound editor Snd.
--with-module=NAME    load the module NAME before to create the executable.
--save-options=OPTS   further arguments for SAVE-LISP-AND-DIE.
--sbcl-options=OPTS   options for SBCL.
--before-save=FORM    read and evaluate FORM before to create the executable.
```

For example:

```
sh install_executable --with-linedit --with-ladspa --with-snd  \
                      --with-module=ltk --with-module=dsp-collection
```

or with Swank server and other options:

```
sh install_executable --with-linedit --with-fluidsynth \
                      --with-ladspa --with-lv2 --with-snd \
                      --swank-loader=/path/to/slime/swank-loader.lisp \
                      --with-module=inferior-shell \
                      --before-save="(use-package 'inferior-shell :scratch)"
```

Note: the support for LV2 plugins requires [Lilv](http://drobilla.net/software/lilv),
a LV2 host library.

If you use [SLIME](https://github.com/slime/slime), add these lines to
`${HOME}/.swank.lisp` if necessary:

```
;; Avoid problems in realtime because the default value is :SPAWN (each
;; request in a separate thread)
#+sbcl (setf swank:*communication-style* :sigio)
;; Useful to redirect standard output from any other threads into the REPL
#+sbcl (defparameter swank:*globally-redirect-io* t)
```

There is also a major mode for GNU Emacs. If you want to install it,
add the following lines to your `.emacs` file:

```
(push "/path/to/incudine/contrib/editors/emacs" load-path)
(require 'incudine)
;; org-babel functions
(require 'ob-incudine)
```

## Documentation

[Incudine Manual](http://incudine.sourceforge.net/incudine.html)

## Tutorials

[Incudine home page](http://incudine.sourceforge.net)

The source files are in [incudine/doc/tutorials/](doc/tutorials/) (text format)
and [incudine/doc/html/](doc/html/) (html format).
