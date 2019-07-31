# vi: ft=sh fdm=marker

stty -ixon # Disable CTRL-S shortcut for freezing terminal.

# Source profile {{{1
################################################################

[[ -f $HOME/.profile ]] && source $HOME/.profile

# Remove path {{{1
################################################################

function remove_path {
	path_pattern=$1
	path=$2

#	IFS=':' read -a arr <<< "$path"
#	for i in "${!arr[@]}" ; do
#		if [ "${arr[i]}" = "$path_pattern" ] ; then
#			unset arr[i]
#			break
#		fi
#	done

#	# Look at start of string
	new_path=$(echo $path | sed "s!^$path_pattern:!!")
	if [ "$new_path" = "$path" ] ; then
		# Look in the middle of the string
		new_path=$(echo $path | sed "s!:$path_pattern:!:!")
		if [ "$new_path" = "$path" ] ; then
			# Look at end of the string
			new_path=$(echo $path | sed "s!:$path_pattern\$!!")
			if [ "$path_pattern" = "$path" ] ; then
				new_path=
			fi
		fi
	fi

	echo $new_path
}



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

#ant_logger_path=$HOME/dev/ant-lib/build
#ant_logger=$ant_logger_path/fr/cea/lib/AntLogger.class
#if [ -e $ant_logger ] ; then
#		export ANT_ARGS="-logger fr.cea.lib.AntLogger"
#		export CLASSPATH=$(concat_paths "$CLASSPATH" $ant_logger_path)
#fi

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


# Homebrew {{{1
################################################################

if [ -d /usr/local/Cellar ] ; then
	homebrew_dir=/usr/local
	export PATH=$(remove_path $homebrew_dir/bin "$PATH")
	export PATH=$(concat_paths $homebrew_dir/bin "$PATH")
	if [ -d $homebrew_dir/sbin ] ; then
		export PATH=$(concat_paths $homebrew_dir/sbin "$PATH")
	fi
	export PKG_CONFIG_PATH=$(concat_paths $homebrew_dir/lib/pkgconfig "$PKG_CONFIG_PATH")
fi



# X11 {{{1
################################################################

x11_home=/usr/X11
if [ -e $x11_home ] ; then
	export LIBRARY_PATH=$(concat_paths $x11_home/lib "$LIBRARY_PATH")
	export CPATH=$(concat_paths $x11_home/include "$CPATH")
fi

# Source bashrc {{{1
################################################################

[[ $- == *i* && -f $HOME/.bashrc ]] && source $HOME/.bashrc
