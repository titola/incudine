OS          = $(shell uname)
HAVE_JACK   = $(shell pkg-config jack && echo 1)
HAVE_X11    = $(shell pkg-config x11 && echo 1)
HAVE_SIMD   = 1
SAMPLE_SIZE = 64
LIBNAME     = libincudine
LIBEXT      = so
CC          = gcc
CFLAGS      = -O3 -Wall -fPIC
LDFLAGS     = -shared -lpthread -lm -lportaudio
PA_CFLAGS   =
TLSF_CFLAGS = -DTLSF_STATISTIC=1
OBJECTS     = tlsf.o util.o rtpa.o

ifeq ($(OS), Linux)
	CFLAGS += -DLINUX
else ifeq ($(OS), Darwin)
	LIBEXT  = dylib
else ifeq ($(OS), CYGWIN)
	LIBNAME = cygincudine-0
	LIBEXT  = dll
endif

ifeq ($(HAVE_JACK), 1)
	PA_CFLAGS += -DPA_HAVE_JACK
endif

ifeq ($(HAVE_X11), 1)
	LDFLAGS += -lX11
	OBJECTS += mouse.o
endif

ifeq ($(HAVE_SIMD), 1)
	TLSF_CFLAGS += -DBLOCK_ALIGN=32
endif

ifeq ($(SAMPLE_SIZE), 64)
	CFLAGS += -D__INCUDINE_USE_64_BIT_SAMPLE__
	LISP_SAMPLE_TYPE = double-float
else
	LISP_SAMPLE_TYPE = single-float
endif

all: $(LIBNAME).$(LIBEXT) sample-type

$(LIBNAME).$(LIBEXT): $(OBJECTS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(OBJECTS)

rtpa.o: rtpa.c
	$(CC) $(CFLAGS) $(PA_CFLAGS) -c $<

tlsf.o: ../contrib/tlsf/src/tlsf.c
	$(CC) $(CFLAGS) $(TLSF_CFLAGS) -c $<

%.o: %.c
	$(CC) $(CFLAGS) -c $<

sample-type:
	@echo '(defvar incudine.util:*sample-type*' "'$(LISP_SAMPLE_TYPE))" > $@.lisp
	@echo -n "$@.lisp: "
	@cat $@.lisp

clean:
	@rm -f $(LIBNAME).$(LIBEXT) *.o sample-type.lisp
