# -*- Makefile-gmake -*-
# vi: fdm=marker

# Constants {{{1
################################################################

# OS type {{{2
PLATFORM=$(shell uname)

# Host name {{{2
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

# Sub-directories {{{2
dirs = vi

# Default target {{{1
################################################################

all:

# Install {{{1
################################################################

install: office versioning dev screen shell imgbg $(HOME)/.mplayer/config

$(HOME)/.mplayer/config: mplayer.conf $(HOME)/.mplayer
	ln -sf $(CURDIR)/$< $@

$(HOME)/.mplayer:
	mkdir -$@

# Uninstall, test and clean {{{1
################################################################

uninstall:
	# TODO clean all config files ($(HOME)/.*)

# Shell {{{1
################################################################

shell: $(HOME)/.bashrc $(HOME)/.profile $(HOME)/.bash_profile

$(HOME)/.profile: profile
	ln -sf $(CURDIR)/$< $@

$(HOME)/.bash_profile: bash_profile
	ln -sf $(CURDIR)/$< $@

#$(HOME)/.xsessionrc: xsessionrc
#	ln -sf $(CURDIR)/$< $@

$(HOME)/.bashrc: bashrc
	ln -sf $(CURDIR)/$< $@

# Screen {{{1
################################################################

screen: $(HOME)/.screenrc $(HOME)/.tmux.conf $(HOME)/.tmux-plugins

$(HOME)/.screenrc: screenrc
	ln -sf $(CURDIR)/$< $@

$(HOME)/.tmux.conf: tmux.conf
	ln -sf $(CURDIR)/$< $@

$(HOME)/.tmux-plugins: tmux-plugins
	ln -sf $(CURDIR)/$< $@

# Versioning {{{1
################################################################

versioning: $(HOME)/.gitconfig $(HOME)/.gitignore

$(HOME)/.gitignore: gitignore
	ln -sf $(CURDIR)/$< $@

$(HOME)/.gitconfig: gitconfig
	ln -sf $(CURDIR)/$< $@

# Web {{{1
################################################################

office: $(HOME)/.lynxrc

$(HOME)/.lynxrc: lynxrc
	ln -sf $(CURDIR)/$< $@

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

# Screen saver & desktop backgrounds {{{1
################################################################

ifeq ($(HOSTPICT_LINK),)
imgbg:
else
imgbg: $(HOSTPICT)

$(HOSTPICT):
	mkdir -p "$(dir $@)"
	wget -O - "$(HOSTPICT_LINK)" | convert - $@
	convert -scale 400 $@ $(dir $@)/$(HOSTNAME)-big.png
endif

# Phony targets {{{1
################################################################

.PHONY: install all uninstall office versioning shell dev imgbg
