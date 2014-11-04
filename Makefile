BUNDLE       = PLP-TP2.tar.gz
BUNDLE_DIR   = PLP-TP2
BUNDLE_FILES = tp2.pl tex Makefile README.md enunciado.pdf informe.pdf

.PHONY: all clean bundle

all: informe.pdf

informe.pdf:
	make -C tex all
	mv tex/informe.pdf .

bundle: clean informe.pdf
	mkdir $(BUNDLE_DIR)
	cp $(BUNDLE_FILES) $(BUNDLE_DIR) -r
	tar zcf $(BUNDLE) $(BUNDLE_DIR)
	rm -rf $(BUNDLE_DIR)

clean:
	make -C tex clean
	rm -rf informe.pdf $(BUNDLE) $(BUNDLE_DIR)
