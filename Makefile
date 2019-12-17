# -*- Makefile-gmake -*-
# vi: fdm=marker

BINARIES=view_csv view_tsv get-battery-charge view_vcal
ROOT_CFG=signature screenrc tmux.conf tmux-plugins bashrc profile bash_profile xsessionrc

# Constants {{{1
################################################################

# OS type
PLATFORM=$(shell uname)

# Host name
HOSTNAME=$(shell hostname | sed 's/[-.].*$$//')

# Host picture {{{2
ifeq ($(HOSTNAME),marcie)
  HOSTPICT_LINK="https://upload.wikimedia.org/wikipedia/en/2/24/Marcie_from_Peanuts.png"
endif
ifeq ($(HOSTNAME),shermy)
  HOSTPICT_LINK="https://vignette2.wikia.nocookie.net/peanuts/images/8/8a/Shermy.gif/revision/latest?cb=20130702002256"
endif
ifeq ($(HOSTNAME),patty)
  HOSTPICT_LINK="https://vignette3.wikia.nocookie.net/peanuts/images/2/2f/Patty.gif/revision/latest?cb=20130702010542"
endif
ifeq ($(HOSTNAME),schroeder)
  HOSTPICT_LINK="https://s-media-cache-ak0.pinimg.com/originals/79/fd/6f/79fd6f3cefb106575a518c0ced91ac13.png"
endif
ifneq ($(HOSTPICT_LINK),)
  HOSTPICT=$(HOME)/Pictures/bg/$(HOSTNAME).png
endif


# Default target {{{1
################################################################

all:

# Install {{{1
################################################################

install: office versioning dev imgbg $(HOME)/.mplayer/config $(HOME)/.config/redshift.conf x11 $(addprefix $(HOME)/.,$(ROOT_CFG)) $(addprefix $(HOME)/bin/,$(BINARIES))

# Folders
$(addprefix $(HOME)/,.mplayer .config bin):
	mkdir -p "$@"

$(HOME)/.%: %
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/.config/%: % $(HOME)/.config
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/bin/%: % $(HOME)/bin
	ln -sf "$(CURDIR)/$<" $@

$(HOME)/.mplayer/config: mplayer.conf $(HOME)/.mplayer
	ln -sf "$(CURDIR)/$<" "$@"

# Uninstall, test and clean {{{1
################################################################

uninstall:
	# TODO clean all config files ($(HOME)/.*)


# Versioning {{{1
################################################################

ifeq ($(PLATFORM),Linux)
versioning: $(HOME)/.gitconfig $(HOME)/.gitignore $(HOME)/.gitlinux
else
versioning: $(HOME)/.gitconfig $(HOME)/.gitignore
endif

# Web {{{1
################################################################

office: $(HOME)/.lynxrc $(HOME)/.lynx/bookmarks.html $(HOME)/.lynx/lynx.cfg $(HOME)/.links/bookmarks.html

$(HOME)/.lynxrc: lynxrc
	ln -sf $(CURDIR)/$< $@

$(HOME)/.lynx/lynx.cfg: lynx.cfg $(HOME)/.lynx
	ln -sf $(CURDIR)/$< $@

$(HOME)/.lynx/bookmarks.html: lynx_bookmarks.html $(HOME)/.lynx
	ln -sf $(CURDIR)/$< $@

$(HOME)/.links/bookmarks.html: links_bookmarks.html $(HOME)/.links
	ln -sf $(CURDIR)/$< $@

$(HOME)/.lynx:
	mkdir -p "$@"

$(HOME)/.links:
	mkdir -p "$@"

# Development {{{1
################################################################

dev: $(HOME)/.Rprofile $(HOME)/.vimrc $(HOME)/.vim $(HOME)/tmp/vim.swp $(HOME)/tmp/vim.bkp

$(HOME)/.Rprofile: Rprofile
	ln -sf $(CURDIR)/$< $@

$(HOME)/.vimrc: vimrc
	ln -sf $(CURDIR)/$< $@

$(HOME)/.vim: vim
	ln -sf $(CURDIR)/$< $@

$(HOME)/tmp/vim.swp:
	mkdir -p $@

$(HOME)/tmp/vim.bkp:
	mkdir -p $@

# X11 {{{1
################################################################

ifeq ($(PLATFORM),Linux)
x11: $(HOME)/.XCompose
else
x11:
endif

# Screen saver & desktop backgrounds {{{1
################################################################

CONVERT=$(which convert)
ifeq ($(CONVERT),)
imgbg:
else
ifeq ($(HOSTPICT_LINK),)
imgbg:
else
imgbg: $(HOSTPICT)

$(HOSTPICT):
	mkdir -p "$(dir $@)"
	wget -O - "$(HOSTPICT_LINK)" | convert - $@
	convert -scale 400 $@ $(dir $@)/$(HOSTNAME)-big.png
endif
endif

# Phony targets {{{1
################################################################

.PHONY: install all uninstall office versioning shell dev imgbg x11
