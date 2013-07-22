OS          = $(shell uname)
HAVE_JACK   = $(shell pkg-config jack && echo 1)
HAVE_PA     = $(shell pkg-config portaudio-2.0 && echo 1)
HAVE_X11    = $(shell pkg-config x11 && echo 1)
HAVE_SIMD   = 1
SAMPLE_SIZE = 64
LIBNAME     = libincudine
LIBEXT      = so
CC          = gcc
CFLAGS      = -O3 -Wall -fPIC
LDFLAGS     = -shared -lpthread -lm
PA_CFLAGS   =
TLSF_CFLAGS = -DTLSF_STATISTIC=1
OBJECTS     = tlsf.o util.o

ifeq ($(OS), Linux)
	CFLAGS += -DLINUX
else ifeq ($(OS), Darwin)
	LIBEXT  = dylib
else ifeq ($(OS), CYGWIN)
	LIBNAME = cygincudine-0
	LIBEXT  = dll
endif

ifeq ($(HAVE_JACK), 1)
	OBJECTS += rtjack.o
	LDFLAGS += -ljack
	DEFAULT_AUDIO_DRIVER = jack
else ifneq ($(HAVE_PA), 1)
	OBJECTS += audio-driver-missing
endif

ifeq ($(HAVE_PA), 1)
	ifeq ($(HAVE_JACK), 1)
		PA_CFLAGS += -DPA_HAVE_JACK
	else
		DEFAULT_AUDIO_DRIVER = portaudio
	endif
	OBJECTS += rtpa.o
	LDFLAGS += -lportaudio
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

all: $(LIBNAME).$(LIBEXT) audio-driver sample-type.lisp

$(LIBNAME).$(LIBEXT): $(OBJECTS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(OBJECTS)

rtjack.o: rtjack.c
	$(CC) $(CFLAGS) -c $<

rtpa.o: rtpa.c
	$(CC) $(CFLAGS) $(PA_CFLAGS) -c $<

tlsf.o: ../contrib/tlsf/src/tlsf.c
	$(CC) $(CFLAGS) $(TLSF_CFLAGS) -c $<

%.o: %.c
	$(CC) $(CFLAGS) -c $<

audio-driver:
	@echo '(defparameter incudine.util:*audio-driver*' ":$(DEFAULT_AUDIO_DRIVER))" > $@.lisp
	@echo -n "$@.lisp: "
	@cat $@.lisp

audio-driver-missing:
	@echo [ERROR] requires jack or portaudio
	@exit 1

sample-type.lisp: util.o
	@echo '(defparameter incudine.util:*sample-type*' "'$(LISP_SAMPLE_TYPE))" > $@
	@echo -n "$@: "
	@cat $@

clean:
	@rm -f $(LIBNAME).$(LIBEXT) *.o sample-type.lisp audio-driver.lisp
