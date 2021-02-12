# vi: ft=sh fdm=marker

# Debug {{{1
################################################################

debug() {
	msg="$1"

	echo "[DEBUG] $msg" >&2
}

# Warn {{{1
################################################################

warning() {
	msg="$1"

	echo "[WARNING] $msg" >&2
}

# Constants {{{1
################################################################

# Debug level
[ -z "$DEBUG" ] && DEBUG=0

# uname
UNAME=$(which uname)
if [ -z "$UNAME" ] ; then
	echo "Cannot find uname program." >&2
	exit 1
fi

# tr
TR=$(which tr)
if [ -z "$TR" ] ; then
	echo "Cannot find tr program." >&2
	exit 1
fi

# awk
AWK=$(which awk)
if [ -z "$AWK" ] ; then
	echo "Cannot find awk program." >&2
	exit 1
fi

# awk
EGREP=$(which egrep)
if [ -z "$EGREP" ] ; then
	echo "Cannot find egrep program." >&2
	exit 1
fi

# sed
SED=$(which sed)
if [ -z "$SED" ] ; then
	echo "Cannot find sed program." >&2
	exit 1
fi

# OS type
PLATFORM=
if [ -n "$UNAME" ] ; then
	PLATFORM=$($UNAME)
fi
[ "$DEBUG" -eq 0 ] || debug "PLATFORM=$PLATFORM"

# Hostname
export HOSTNAME=$(hostname | sed 's/[-.].*$//')
[ "$DEBUG" -eq 0 ] || debug "HOSTNAME=$HOSTNAME"

# Linux distribution
[ "$PLATFORM" = Linux -a -e /proc/version -a -n "$SED" ] && DISTRIBUTION=$($SED 's/^.*(\([^ ]*\).*$/\1/' /proc/version)
[ "$DEBUG" -eq 0 ] || debug "DISTRIBUTION=$DISTRIBUTION"

# Remove from path {{{1
################################################################

remove_from_path () {
	local path="$1"
	local pattern="$2"

	path=$(echo $path | $TR ":" "\n" | $EGREP -v "^$pattern$" | $TR "\n" ":" | $SED 's/:$//')

	echo "$path"
}

# Concat paths {{{1
################################################################

concat_paths () {

	local path1=$1
	local path2=$2

	# Concatenate
	local ret_path=$path1:$path2

	# Remove duplicates and blank paths
	ret_path=$(echo $ret_path | $TR ":" "\n" | $AWK '!x[$0]++' | $SED '/^$/d' | $TR "\n" ":" | $SED 's/:$//')

	echo $ret_path
}

# Man path {{{1
################################################################

if [ "$DISTRIBUTION" = Alpine ] ; then
	warning "Don't know how to set MANPATH under Alpine."
else
	export MANPATH=$(man -w)
fi

# Locale {{{1
################################################################

if [ "$PLATFORM" = "Darwin" ] ; then
	export LC_ALL=en_US.UTF-8
	export LANG=en_US.UTF-8
fi

# New file mask {{{1
################################################################

umask 077


# Editor {{{1
################################################################

vim=$(which vim 2>/dev/null)
if [ -n "$vim" ] ; then
	export EDITOR=$vim
fi

# User binaries {{{1
################################################################

user_bin=$HOME/bin
if [ -d $user_bin ] ; then
	export PATH=$(concat_paths $user_bin "$PATH")
fi

# Private profile {{{1
################################################################

private_profile="$HOME/.private.profile"
[ -f "$private_profile" ] && source "$private_profile"

# Aspera / ascp {{{1
################################################################

case "$PLATFORM" in
	Linux)  aspera_dir=$HOME/.aspera/cli ;;
	Darwin) aspera_dir=$HOME/Applications/Aspera\ CLI ;;
esac
if [ -d "$aspera_dir" ] ; then
	export PATH=$(concat_paths "$aspera_dir/bin" "$PATH")
	export MANPATH=$(concat_paths "$aspera_dir/share/man" "$MANPATH")
fi

# Dia {{{1
################################################################

if [ "$PLATFORM" = "Darwin" ] ; then
	dia_app_bin=/Applications/Dia.app/Contents/Resources/bin
	if [ -d $dia_app_bin ] ; then
		export PATH=$(concat_paths "$dia_app_bin" "$PATH")
	fi
fi

# Homebrew {{{1
################################################################

# Search for Linux configuration
homebrew_dir=$HOME/.linuxbrew
if [ -d "$homebrew_dir" ] ; then
	export PATH="/home/pierrick/.linuxbrew/bin:$PATH"
	export MANPATH="/home/pierrick/.linuxbrew/share/man:$MANPATH"
	export INFOPATH="/home/pierrick/.linuxbrew/share/info:$INFOPATH"
elif [ -d /usr/local/Cellar ] ; then
	homebrew_dir=/usr/local
	export PATH=$(remove_from_path "$PATH" $homebrew_dir/bin)
	export PATH=$(concat_paths $homebrew_dir/bin "$PATH")
	if [ -d $homebrew_dir/sbin ] ; then
		export PATH=$(remove_from_path "$PATH" $homebrew_dir/sbin)
		export PATH=$(concat_paths $homebrew_dir/sbin "$PATH")
	fi
	export PKG_CONFIG_PATH=$(concat_paths $homebrew_dir/lib/pkgconfig "$PKG_CONFIG_PATH")
fi

# Lynx {{{1
################################################################

lynx_cfg=$HOME/.lynx/lynx.cfg
if [ -f "$lynx_cfg" ] ; then
	export LYNX_CFG="$lynx_cfg"
fi

# Msmtpq {{{1
################################################################

if [ -n "$(which msmtp 2>/dev/null)" -a '(' -z "$(which msmtpq 2>/dev/null)" -o -z "$(which msmtp-queue 2>/dev/null)" ')' ] ; then
	msmtp_path=$(which msmtp)
	if [ "$PLATFORM" = "Darwin" -a -n "$msmtp_path" ] ; then
		msmtpq_script_dir=$(dirname $msmtp_path)/$(dirname $(readlink $msmtp_path))/../share/msmtp/scripts/msmtpq
	elif [ "$PLATFORM" = "Linux" ] ; then
		msmtpq_script_dir="/usr/share/doc/msmtp/examples/msmtpq"
		[[ -d $msmtpq_script_dir ]] || msmtpq_script_dir="/usr/share/doc/msmtp/msmtpq"
	fi
	if [ -d "$msmtpq_script_dir" ] ; then
		export PATH=$(concat_paths $msmtpq_script_dir "$PATH")
	fi
fi


# Perl {{{1
################################################################

perl5_dir=$HOME/perl5
if [ -d $perl5_dir ] ; then
	export MANPATH=$(concat_paths $perl5_dir/man "$MANPATH")
	export PATH=$(concat_paths $perl5_dir/bin "$PATH")
	export PERL5LIB=$(concat_paths $perl5_dir/lib/perl5 $PERL5LIB)
	export PERL_LOCAL_LIB_ROOT=$(concat_paths $perl5_dir $PERL_LOCAL_LIB_ROOT)
	export PERL_MB_OPT="--install_base \"$perl5_dir\"";
	export PERL_MM_OPT="INSTALL_BASE=$perl5_dir";
fi


# Python {{{1
################################################################

for python_dir in $HOME/Library/Python/* $HOME/.local ; do
	if [ -d $python_dir/bin ] ; then
		export PATH=$(concat_paths $python_dir/bin "$PATH")
	fi
done

if which pyenv >/dev/null 2>&1 ; then
	eval "$(pyenv init -)"
fi

# Conda {{{2
PATH=$(remove_from_path "$PATH" "miniconda")
unset MINICONDABIN
for d in $HOME /usr/local ; do
	d="$d/miniconda3/bin"
	if [ -d $d ] ; then
		export MINICONDABIN=$d      # For what?
		export PATH=$(concat_paths "$d" "$PATH")
	fi
done

# R {{{1
################################################################

# Use CRAN R studio version if present
if [ "$PLATFORM" = "Darwin" ] ; then
	r_framework_bin=/Library/Frameworks/R.framework/Versions/Current/Resources/bin
	if [ -d $r_framework_bin ] ; then
		export PATH=$(concat_paths "$PATH" "$r_framework_bin")
	fi
fi

# Set R_HOME and library directory
R=$(which R 2>/dev/null)
if [ -n "$R" ] ; then
	R --slave --no-restore -e 'dir.create(Sys.getenv()[["R_LIBS_USER"]], recursive = TRUE, showWarnings = FALSE)'
	export R_HOME=$(R --slave --no-restore RHOME)
fi

# Set R_BROWSER
if [ "$PLATFORM" = "Darwin" ] ; then
	export R_BROWSER=open
else
	export R_BROWSER=xdg-open
fi

# RSync {{{1
################################################################

export RSYNC_RSH=ssh

# Ruby {{{1
################################################################

if which ruby >/dev/null 2>/dev/null && which gem >/dev/null 2>/dev/null; then
	gem_dir=$(ruby -r rubygems -e 'puts Gem.user_dir')
	export PATH=$(concat_paths $gem_dir/bin "$PATH")
fi

# Screen {{{1
################################################################

export SCREEN_BG_COLOR=k
export SCREEN_FG_COLOR=w
case "$HOSTNAME" in
#	marcie) SCREEN_BG_COLOR=Y SCREEN_FG_COLOR=k ;;
#	patty) SCREEN_BG_COLOR=r SCREEN_FG_COLOR=w ;;
#	shermy) SCREEN_BG_COLOR=y SCREEN_FG_COLOR=k ;;
#	schroeder) SCREEN_BG_COLOR=B ; SCREEN_FG_COLOR=W ;;
#	is*) SCREEN_BG_COLOR=R ; SCREEN_FG_COLOR=W ;;
esac

# Versioning {{{1
################################################################

# Environment variable for scan-repos
export REPOSPATH=
[[ -d "$HOME/dev" ]] && REPOSPATH=$(concat_paths "$REPOSPATH" "$HOME/dev")
[[ -d "$HOME/private" ]] && REPOSPATH=$(concat_paths "$REPOSPATH" "$HOME/private/dev")

# MPI {{{1
################################################################

# On DM2I DIGITEO Cluster
if [ "$HOSTNAME" = "is227470" ] ; then
	export MPI_HOST=ibnode0
fi
# RDKit {{{1
################################################################

rdkit_dir=/usr/local/share/RDKit
if [ -e $rdkit_dir ] ; then
	export RDBASE=/usr/local/share/RDKit
fi

# NVIDIA CUDA {{{1
################################################################

# if [ "$PLATFORM" = "darwin" ] ; then
# 	for version in 5.0 4.0 ; do # prefer version 4.0, for open-mpi compatibility (open-mpi doesn't support 5.0 yet).
# 		cuda_home=/Developer/NVIDIA/CUDA-$version
# 		if [ -d $cuda_home ] ; then
# 			break
# 		fi
# 	done
# else
# 		cuda_home=/usr/local/cuda
# fi

# Take latest version of CUDA
#cuda_home=$(/bin/ls -d1 /Developer/NVIDIA/CUDA-* | sort | tail -n 1)
cuda_home=/usr/local/cuda

# Set env vars
if [ -e $cuda_home ] ; then
	PATH=$(concat_paths $cuda_home/bin "$PATH")
	export LIBRARY_PATH=$(concat_paths $cuda_home/lib "$LIBRARY_PATH")
	export CPATH=$(concat_paths $cuda_home/include "$CPATH")
#	if [ "$PLATFORM" = "darwin" ] ; then
#export DYLD_LIBRARY_PATH=$(concat_paths $cuda_home/lib "$DYLD_LIBRARY_PATH")
#		export DYLD_FALLBACK_LIBRARY_PATH=$(concat_paths "$DYLD_FALLBACK_LIBRARY_PATH" $cuda_home/lib)
#	else
#		export LD_LIBRARY_PATH=$(concat_paths $cuda_home/lib "$LD_LIBRARY_PATH")
#	fi
fi

# Ant {{{1
################################################################

if [ "$PLATFORM" = "cygwin" ] ; then
	ANT_HOME=$(echo /cygdrive/c/apache*)
	if [ -d "$ANT_HOME/bin" ] ; then
		export ANT_HOME
		export PATH=$(concat_paths "$ANT_HOME/bin" "$PATH")
	fi
fi

# Plant UML task {{{2

if [ "$PLATFORM" = "darwin" ] ; then
	plantuml_installed=$(which plantuml)
	if [ -n "$plantuml_installed" ] ; then
		export CLASSPATH=$(concat_paths "$(brew --prefix plantuml)/libexec/plantuml.jar" "$CLASSPATH")
	fi
fi

# CYGWIN {{{1
################################################################

if [ "$PLATFORM" = "cygwin" ] ; then
		# `find` has a homonym under Windows: C:\Windows\System32\find
		find=/usr/bin/find
fi

# Java {{{1
################################################################

# To allow display of AWT/Swing GUI on xmonad or other tiling window managers.
export _JAVA_AWT_WM_NONREPARENTING=1

# Use java_home executable
jh=/usr/libexec/java_home
if [ -x $jh ] ; then
	JAVA_HOME=$($jh 2>/dev/null)
	if [ -n "$JAVA_HOME" ] ; then
		export JAVA_HOME
		export JAVA_CPPFLAGS="-I$JAVA_HOME/include -I$JAVA_HOME/include/darwin"
		export JAVA_LIBS="-L$JAVA_HOME/jre/lib/server -L$JAVA_HOME/jre/lib -ljvm"
		export JAVA_LD_LIBRARY_PATH="$JAVA_HOME/jre/lib/server:$JAVA_HOME/jre/lib"
	fi
fi

# Linux
jh=/usr/lib/jvm/default-java
if [ -d $jh ] ; then
	export JAVA_HOME=$jh
fi

# Cygwin
if [ "$PLATFORM" = "cygwin" ] ; then
	if [ -z "$JAVA_HOME" ] ; then
		for java_home_unix in /cygdrive/c/Java/jdk* /cygdrive/c/Program\ Files*/Java/jdk* ; do
			if [ -d "$java_home_unix" ] ; then
				export JAVA_HOME=`cygpath -d "$java_home_unix"`
				break
			fi
		done
	fi
	if [ -n "$JAVA_HOME" ] ; then
		export PATH=$(concat_paths "$JAVA_HOME/bin" "$PATH")
	fi
fi

# MATLAB {{{1
################################################################

if [ "$PLATFORM" = "darwin" ] ; then

		matlab_bin=$(which /Applications/MATLAB*.app/bin/matlab | tail -n 1)
		if [ -n "$matlab_bin" ] ; then
				matlab_dir=$(dirname $matlab_bin)
				export PATH=$(concat_paths $matlab_dir "$PATH")
		fi
fi



# MONO {{{1
################################################################

if [ "$PLATFORM" = "darwin" ] ; then
	mono_framework_dir=/Library/Frameworks/Mono.framework
	if [ -d $mono_framework_dir ] ; then
		export LD_LIBRARY_PATH=$(concat_paths $mono_framework_dir/Versions/Current/lib "$LD_LIBRARY_PATH")
	fi
fi

# PHP PEAR HOME {{{1
################################################################

pear_home=$HOME/pear

if [ -e $pear_home ] ; then

	# PHP.INI
	phpdir="$HOME/.php"
	if [ ! -e $phpdir ] ; then
		mkdir $phpdir
		cat >>"$phpdir/php.ini" <<EOF
include_path=".:$pear_home/share/pear"
EOF
	fi

	# ENV VARS
	export PATH=$(concat_paths $pear_home/bin "$PATH")
	export PHPRC=$phpdir
fi

# LATEX {{{1
################################################################

if [ "$PLATFORM" = "darwin" ] ; then
	for version in 2012 2011 ; do
		texlive=/usr/local/texlive/$version
		if [ -d $texlive ] ; then
			export PATH=$(concat_paths $texlive/bin/x86_64-darwin "$PATH")
			export MANPATH=$(concat_paths $texlive/texmf/doc/man "$MANPATH")
			export TEXINPUTS=$(concat_paths $texlive/texmf-dist// "$TEXINPUTS")
			export TEXINPUTS=$(concat_paths . "$TEXINPUTS")
			break
		fi
	done
fi    

# SCILAB {{{1
################################################################

if [ "$PLATFORM" = "darwin" ] ; then

		scilab_bin=$(which /Applications/scilab*.app/Contents/MacOS/bin/scilab | tail -n 3)
		if [ -n "$scilab_bin" ] ; then
				scilab_dir=$(dirname $scilab_bin)
				export PATH=$(concat_paths $scilab_dir "$PATH")
		fi
fi

# MYSQL {{{1
################################################################

# MYSQL C++ CONNECTOR
mysql_cpp_conn_home=/usr/local/Cellar/mysql-connector-c++/1.1.0
if [ -e $mysql_cpp_conn_home ] ; then
	export LIBRARY_PATH=$(concat_paths $mysql_cpp_conn_home/lib "$LIBRARY_PATH")
	export CPATH=$(concat_paths $mysql_cpp_conn_home/include "$CPATH")
#	if [ "$PLATFORM" = "darwin" ] ; then
#		export DYLD_FALLBACK_LIBRARY_PATH=$(concat_paths "$DYLD_FALLBACK_LIBRARY_PATH" $mysql_cpp_conn_home/lib)
#	else
#		export LD_LIBRARY_PATH=$(concat_paths $mysql_cpp_conn_home/lib "$LD_LIBRARY_PATH")
#	fi
fi

# BOOST {{{1
################################################################

boost_home=/usr/local/Cellar/boost/1.54.0
if [ -e $boost_home ] ; then
	export LIBRARY_PATH=$(concat_paths $boost_home/lib "$LIBRARY_PATH")
	export CPATH=$(concat_paths $boost_home/include "$CPATH")
#	if [ "$PLATFORM" = "darwin" ] ; then
#		export DYLD_FALLBACK_LIBRARY_PATH=$(concat_paths "$DYLD_FALLBACK_LIBRARY_PATH" $boost_home/lib)
#	else
#		export LD_LIBRARY_PATH=$(concat_paths $boost_home/lib "$LD_LIBRARY_PATH")
#	fi
fi

# CMAKE {{{1
################################################################

if [ "$PLATFORM" = "darwin" ] ; then
	cmake_app=/Applications/CMake.app/Contents/
	if [ -d $cmake_app ] ; then
		export PATH=$(concat_paths "$cmake_app/bin" "$PATH")
		export MANPATH=$(concat_paths "$cmake_app/man" "$MANPATH")
	fi
fi

# TCL/TK {{{1
################################################################

if [ "$PLATFORM" = "darwin" ] ; then

		tclsh_bin=$(which /usr/local/Cellar/tcl-tk/*/bin/tclsh | tail -n 1)
		if [ -n "$tclsh_bin" ] ; then
				tcltk_dir=$(dirname $tclsh_bin)
				export PATH=$(concat_paths $tcltk_dir "$PATH")
		fi
fi


# Haskell {{{1
################################################################

# Cabal binaries
cabal_folder=$HOME/.cabal
if [ -d "$cabal_folder" -a -d "$cabal_folder/bin" ] ; then
	export PATH=$(concat_paths "$PATH" "$cabal_folder/bin")
fi

# countdown {{{1
################################################################

file=~/data/sounds/gong.wav
[[ -f $file ]] && export COUNTDOWN_SOUND_FILE="$file"

# X11 {{{1
################################################################

export TERMINAL=urxvt

x11_home=/usr/X11
if [ -e $x11_home ] ; then
	export LIBRARY_PATH=$(concat_paths $x11_home/lib "$LIBRARY_PATH")
	export CPATH=$(concat_paths $x11_home/include "$CPATH")
fi

# Disable CTRL-S freezing terminal {{{1
################################################################

if [ "$SHELL" = '/bin/bash' ] ; then
	stty -ixon # Disable CTRL-S shortcut for freezing terminal.
fi
