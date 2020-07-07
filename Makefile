# -*- Makefile-gmake -*-
# vi: fdm=marker

BINARIES=view_csv view_tsv view_vcal termcolors amutt gmutt runx termprg sysinfo vpncea
ROOT_CFG=signature screenrc tmux.conf tmux-plugins bashrc zshrc profile bash_profile zprofile xsessionrc xinitrc newsboat links lynx lynxrc i3status.conf Xmodmap Xresources xmonad xmobarrc mplayer cabal xbindkeysrc xscreensaver
CONFIG_DIR=redshift.conf canto i3 vimb

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

install: versioning dev imgbg x11 $(addprefix $(HOME)/.config/,$(CONFIG_DIR)) $(addprefix $(HOME)/.,$(ROOT_CFG)) $(addprefix $(HOME)/bin/,$(BINARIES))

$(HOME)/.%: %
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/.bashrc: shellrc
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/.zshrc: shellrc
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/.bash_profile: profile
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/.zprofile: profile
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/.config/%: %
	@mkdir -p $(HOME)/.config
	ln -sf "$(CURDIR)/$<" "$@"

$(HOME)/bin/%: %
	@mkdir -p $(HOME)/bin
	ln -sf "$(CURDIR)/$<" $@

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
