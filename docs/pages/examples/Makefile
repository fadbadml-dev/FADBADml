ROOT_DIR = ../..

CSS_FILE = $(ROOT_DIR)/css/doc_style.css
HIGHLIGHT_STYLE = pygments

INCLUDES = -c $(CSS_FILE) --highlight-style $(HIGHLIGHT_STYLE)

.PHONY: all update clean

all: update

update: examples.html

%.html: %.md
	pandoc $(INCLUDES) -s $< -o $@

clean:
	rm -f *.html
