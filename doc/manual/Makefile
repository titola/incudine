MAKEINFO = makeinfo --no-split
TEXI2HTML = texi2any --html --no-split --css-ref=incudine-manual.css
TEXI2PDF = texi2dvi -p

INCFILES = copying.org dictionary.org export.org features.org \
           index.org install.org intro.org license.org

INCFILES += $(wildcard cache/*.dict)

all:
	@echo Usage: make targets
	@echo Available targets: html info org pdf undocumented

html: incudine.html

info: incudine.info

org: incudine.org

pdf: incudine.pdf

undocumented: undocumented.pdf

define emacs_eval =
  emacs --batch --eval '(progn (find-file "$<") ($(1)))'
endef

define write_dict =
sbcl --noinform --noprint --disable-debugger \
  --eval '(pushnew :make-incudine-manual *features*)' \
  --load fill-dict.lisp \
  --eval '(incudine.doc:write-incudine-version "version")' \
  --eval '(incudine.doc:write-doc "dictionary.org" "$(1)" :type "$(2)" :out-of-dictionary-file "out-of-dict.lisp")' \
  --eval '(incudine.doc:write-doc "cache/cudere-clm.dict" "cache/cudere-clm.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(incudine.doc:write-doc "cache/fluidsynth.dict" "cache/fluidsynth.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(incudine.doc:write-doc "cache/jack.dict" "cache/jack.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(incudine.doc:write-doc "cache/ladspa.dict" "cache/ladspa.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(incudine.doc:write-doc "cache/lv2.dict" "cache/lv2.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(incudine.doc:write-doc "cache/portaudio.dict" "cache/portaudio.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(incudine.doc:write-doc "cache/portmidi.dict" "cache/portmidi.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(incudine.doc:write-doc "cache/snd.dict" "cache/snd.org" :type "$(2)" :if-exists :supersede)' \
  --eval '(sb-ext:exit)'
endef

define write_undoc =
sbcl --noinform --noprint \
  --eval '(pushnew :make-incudine-manual *features*)' \
  --load fill-dict.lisp \
  --eval '(incudine.doc:write-undocumented-symbols "$(1)" "$(2)")' \
  --eval '(sb-ext:exit)'
echo "* Export Setup :noexport:" >> $@
echo "#+title: Incudine {{{version}}}: undocumented symbols" >> $@
echo "#+subtitle: {{{time(%Y-%m-%d)}}}" >> $@
echo "#+include: version" >> $@
endef

define write_config
echo "#+texinfo: @exampleindent 0" >> $@
echo "#+begin_example" >> $@
cat ../../incudinerc-example >> $@
echo "#+end_example" >> $@
endef

incudine.org: $(INCFILES) fill-dict.lisp
	echo -e "#+title: Incudine\n\n" > incudine.org
	cat copying.org intro.org features.org install.org >> incudine.org
	$(call write_config)
	$(call write_dict,incudine.org,texinfo)
	cat cache/jack.org cache/portaudio.org cache/portmidi.org cache/snd.org cache/cudere-clm.org cache/fluidsynth.org cache/ladspa.org cache/lv2.org cache/emacs.org license.org index.org export.org version >> incudine.org

incudine.texi: incudine.org
	$(call emacs_eval,org-texinfo-export-to-texinfo)
	sed -f fix_texi_conv.sed -i $@

incudine.info: incudine.texi
	$(MAKEINFO) $<
	install-info --info-dir . --max-width=90 $@

incudine.html: incudine.texi
	$(TEXI2HTML) $<
	sed '/="settitle"/a<div class="subtitle">Music/DSP programming environment for Common Lisp</div>' -i $@

incudine.pdf: incudine.texi
	$(TEXI2PDF) $<

undocumented.pdf: undocumented.texi
	$(TEXI2PDF) $<

undocumented.texi: undocumented.org
	$(call emacs_eval,org-texinfo-export-to-texinfo)

undocumented.org: undocumented.dict
	$(call write_undoc,$<,$@)

undocumented.dict: incudine.org
	sed -n '/^* The Incudine dictionary/,/^* /p' $<|sed '$$d'>$@

clean:
	@rm -f dir incudine.* version out-of-dict.lisp undocumented.*
