# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

INSTALL_DIR=$(DESTDIR)/usr
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib/haxe
INSTALL_STD_DIR=$(INSTALL_LIB_DIR)/std
PACKAGE_OUT_DIR=out
PACKAGE_SRC_EXTENSION=.tar.gz

PLATFORM?=unix

OUTPUT=haxe
EXTENSION=

BRANCH=$(shell echo $$APPVEYOR_REPO_NAME | grep -q /haxe && echo $$APPVEYOR_REPO_BRANCH || echo $$TRAVIS_REPO_SLUG | grep -q /haxe && echo $$TRAVIS_BRANCH || git rev-parse --abbrev-ref HEAD)
COMMIT_SHA=$(shell git rev-parse --short HEAD)
COMMIT_DATE=$(shell \
	if [ "$$(uname)" = "Darwin" ]; then \
		date -u -r $$(git show -s --format=%ct HEAD) +%Y%m%d%H%M%S; \
	else \
		date -u -d @$$(git show -s --format=%ct HEAD) +%Y%m%d%H%M%S; \
	fi \
)
PACKAGE_FILE_NAME=haxe_$(COMMIT_DATE)_$(COMMIT_SHA)
HAXE_VERSION=$(shell $(OUTPUT) -version 2>&1 | awk '{print $$1;}')

# using $(CURDIR) on Windows will not work since it might be a Cygwin path
ifdef SYSTEMROOT
	EXTENSION=.exe
else
	export HAXE_STD_PATH=$(CURDIR)/std
endif

haxe: build
	cp _build/src/main.native $(OUTPUT)$(EXTENSION)

haxelib:
	(cd $(CURDIR)/extra/haxelib_src && $(CURDIR)/$(OUTPUT) client.hxml && nekotools boot run.n)
	mv extra/haxelib_src/run$(EXTENSION) haxelib$(EXTENSION)

tools: haxelib

# will install native version of the tools instead of script ones
install_tools: tools
	cp haxelib ${INSTALL_BIN_DIR}/haxelib
	chmod a+rx $(INSTALL_BIN_DIR)/haxelib

# Package

package_src:
	mkdir -p $(PACKAGE_OUT_DIR)
	# use git-archive-all since we have submodules
	# https://github.com/Kentzo/git-archive-all
	curl -s https://raw.githubusercontent.com/Kentzo/git-archive-all/1.15/git_archive_all.py -o extra/git_archive_all.py
	python extra/git_archive_all.py $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_src$(PACKAGE_SRC_EXTENSION)

package_unix:
	mkdir -p $(PACKAGE_OUT_DIR)
	rm -rf $(PACKAGE_FILE_NAME) $(PACKAGE_FILE_NAME).tar.gz
	# Copy the package contents to $(PACKAGE_FILE_NAME)
	mkdir -p $(PACKAGE_FILE_NAME)
	cp -r $(OUTPUT) haxelib$(EXTENSION) std extra/LICENSE.txt extra/CONTRIB.txt extra/CHANGES.txt $(PACKAGE_FILE_NAME)
	# archive
	tar -zcf $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_bin.tar.gz $(PACKAGE_FILE_NAME)
	rm -r $(PACKAGE_FILE_NAME)

package_bin: package_$(PLATFORM)

install_dox:
	haxelib git hxparse https://github.com/Simn/hxparse master src
	haxelib git hxtemplo https://github.com/Simn/hxtemplo
	haxelib git hxargs https://github.com/Simn/hxargs
	haxelib git markdown https://github.com/dpeek/haxe-markdown master src
	haxelib git hxcpp https://github.com/HaxeFoundation/hxcpp
	haxelib git hxjava https://github.com/HaxeFoundation/hxjava
	haxelib git hxcs https://github.com/HaxeFoundation/hxcs
	haxelib git dox https://github.com/HaxeFoundation/dox

package_doc:
	mkdir -p $(PACKAGE_OUT_DIR)
	cd $$(haxelib path dox | head -n 1) && haxe run.hxml && haxe gen.hxml
	haxelib run dox -theme haxe_api -D website "http://haxe.org/" --title "Haxe API" -o $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_doc.zip -D version "$$(haxe -version 2>&1)" -i $$(haxelib path dox | head -n 1)bin/xml -ex microsoft -ex javax -ex cs.internal -D source-path https://github.com/HaxeFoundation/haxe/blob/$(BRANCH)/std/

deploy_doc:
	scp $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_doc.zip www-haxe@api.haxe.org:/data/haxeapi/www/v/dev/api-latest.zip
	ssh www-haxe@api.haxe.org "cd /data/haxeapi/www/v/dev && find . ! -name 'api-latest.zip' -maxdepth 1 -mindepth 1 -exec rm -rf {} + && unzip -q -o api-latest.zip"