BASEURL := https://project.io

# Folders
SOURCE_DIR := src
TEMPLATE_DIR := templates
OUTPUT_HTML_DIR := html
STATIC_DIR := static
SOURCE_FOLDERS := $(shell find $(SOURCE_DIR) -type d)
TARGET_HTML_FOLDERS := $(subst $(SOURCE_DIR),$(OUTPUT_HTML_DIR),$(SOURCE_FOLDERS)) 

# Files
MD_FILES := $(shell find $(SOURCE_DIR) -type f -name "*.md")
TARGET_HTML_FILES := $(patsubst %.md,%.html,$(subst $(SOURCE_DIR),$(OUTPUT_HTML_DIR),$(MD_FILES)))
STATIC_FILES := $(wildcard $(STATIC_DIR)/*)
TARGET_STATIC_FILES := $(subst $(STATIC_DIR),$(OUTPUT_HTML_DIR),$(STATIC_FILES))

BASE_TEMPLATE := default.html 
SIDEBAR_TEMPLATE := toc-sidebar.html
HEADER_HTML := $(TEMPLATE_DIR)/header.html
FOOT_NOTE_HTML := $(TEMPLATE_DIR)/footer.html

# Pandoc
ifeq ($(CI),true)
PANDOC := docker run --volume "`pwd`:/data" --user `id -u`:`id -g` pandoc/latex:2.6
else
PANDOC := pandoc
endif

FLAGS := \
	--standalone \
	--from markdown \
	--to html \
	--mathjax \
	--toc \
	--toc-depth=3 \
	--css=all.css \
	--template=templates/custom.html \
	--include-after-body=$(FOOT_NOTE_HTML) \
	--include-before-body=html/navigation.html \
	--include-in-header=$(HEADER_HTML)
NAV_FLAGS := \
	--from markdown+native_divs \
	--to html

all: $(TARGET_HTML_FOLDERS) $(TARGET_HTML_FILES) $(TARGET_STATIC_FILES)

singlepage: $(TARGET_HTML_FOLDERS) $(TARGET_STATIC_FILES) html/navigation.html html/all.css
	$(PANDOC) $(FLAGS) --output html/full.html $(MD_FILES)

$(TARGET_HTML_FOLDERS):
	mkdir -p $@

html/%.html: $(SOURCE_DIR)/%.md html/navigation.html html/all.css
	$(PANDOC) $(FLAGS) --output $@ $<

html/navigation.html: navigation.md
	$(PANDOC) $(NAV_FLAGS) --output $@ $<

$(TARGET_STATIC_FILES): $(STATIC_FILES)
	cp $< $@

html/all.css:
	cat css/bootstrap.min.css > $(OUTPUT_HTML_DIR)/all.css
	cat css/screen.css >> $(OUTPUT_HTML_DIR)/all.css

clean:
	rm -rf $(OUTPUT_HTML_DIR)/*

.PHONY: all clean
