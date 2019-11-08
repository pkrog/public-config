# vi: fdm=marker

# Load /etc/bashrc {{{1
################################################################

if [ -f /etc/bashrc ] ; then
	. /etc/bashrc
fi

# Blank lines {{{1
################################################################

function blank_lines {

	n=$1

	# Default number of lines
	if [[ -z $n ]] ; then
		n=10
	fi

	# Print blank lines
	for ((i = 0 ; i < n ; ++i)) ; do
		echo
	done
}

# macos FileMerge {{{1
################################################################

file_merge=/Applications/Xcode.app/Contents/Applications/FileMerge.app/Contents/MacOS/FileMerge
if [[ -x $file_merge ]] ; then
	function filemerge {
		file1="$1"
		file2="$2"
		$file_merge -left "$file1" -right "$2" &
	}
fi

# Terminal color {{{1
################################################################

if [[ $COLORTERM == xfce4-terminal || $TERM == xterm-256color-italic || $TERM == screen.xterm-256color ]] ; then
	TERM=xterm-256color
fi

# ls command {{{1
################################################################

if [[ $(uname) == 'Linux' && ( $TERM == xterm-256color || $TERM == xterm || $TERM == screen-256color ) ]] ; then
	alias ls="ls --color"

	# Set colors here, since when logging inside Linux console (so before starting X), TERM will be set to "linux" when parsing "~/.profile".
		eval $(dircolors - <<EOF
# @@@BEGIN_DIRCOLORS@@@
DIR 38;5;45 # Directories
.mk 38;5;92
*Makefile 38;5;92
*CMakeLists.txt 38;5;92
*DESCRIPTION 38;5;208
*LICENSE 38;5;208
*NAMESPACE 38;5;208
*README 38;5;208
*README.md 38;5;208
*.Rmd 38;5;208
*.md 38;5;208
.R  38;5;130
.py 38;5;76
.cfg 38;5;118 # Like setup.cfg for Python.
.c  38;5;213
.sh 38;5;197
.slurm 38;5;13
.dockerfile 38;5;39
*Dockerfile 38;5;39
*Vagrantfile 38;5;39
EXEC 38;5;226;48;5;237
.o 38;5;226;48;5;237
.tar 38;5;220
.tar.gz 38;5;220
.tgz 38;5;220
.zip 38;5;220
.log 38;5;11
.sqlite 38;5;35
.tsv 38;5;43
.csv 38;5;43
.json 38;5;114
.yml 38;5;112
.yaml 38;5;112
.h5 38;5;200
.mat 38;5;200
.html 38;5;217
.xml 38;5;207
.pdf 38;5;219
.xls 38;5;255;48;5;160
.doc 38;5;255;48;5;160
.ppt 38;5;255;48;5;160
.odt 38;5;202
.xlsx 38;5;202
.odp 38;5;202
.dia 38;5;136
.docx 38;5;203
.xlsx 38;5;203
.pptx 38;5;203
.jpg  38;5;226
.png  38;5;226
.gif  38;5;226
.tif  38;5;226
.tiff 38;5;226
.svg  38;5;221
.xcf  38;5;223
# @@@END_DIRCOLORS@@@
EOF
		)
else
	alias ls="ls -F"
fi

# Alias {{{1
################################################################

alias lock='/System/Library/CoreServices/"Menu Extras"/User.menu/Contents/Resources/CGSession -suspend'
#alias vscreen="cd $HOME/dev && screen vim private-notes/todo.md && cd -"
#alias vtmux="cd $HOME/dev && tmux new-session vim private-notes/todo.md && cd -"
alias xterm="xterm -bg black -cr white -fg white"
alias today='date "+%a %b %e %k:%M %Z W%V"'
alias wakeupshermy='wakeonlan 00:26:bb:60:51:16'
alias wakeupschroeder='wakeonlan 3c:07:54:69:be:c0'
alias mailcheck='while true ; do offlineimap && sleep 30 ; done'
alias docker-clean='docker rm $(docker ps -aq) ; docker rmi $(docker images -aq)'
alias runglx='([[ -d .venv ]] || virtualenv .venv) && R_LIBS_USER= && ./run.sh'
alias restartmails="brew services restart offlineimap"

# Tunnels
alias mkdsh-tun='ssh -gNL 30000:192.168.99.100:30000'
alias mkglx-tun='ssh -gNL 30700:192.168.99.100:30700'
alias glx-tun='ssh -gNL 8080:localhost:8080'

# Clusters
alias ssh_cluster='ssh pr228844@is227470.intra.cea.fr'
alias ssh_factoryia='ssh proger@132.167.191.33'

# Top {{{2
if [ "$PLATFORM" = "Linux" ] ; then
	alias top='top'
elif [ "$PLATFORM" = "Darwin" ] ; then
	if [ "`which top 2>/dev/null`" != "/usr/bin/top" ] ; then
		# GNU version of top
		top_head='  PID COMMAND    USER   %CPU   TIME   #TH #PRTS RPRVT  RSHRD  RSIZE   VSIZE #MREGS'
		top_line='$aaaa ^bbbbbbbbb ^nnn $cccc% $wwwwwww $ee $ffff-$hhhh- $iiii- $jjjj-  $llll-$ggggg'
		alias top="top -o-cpu -P '$top_head' -p '$top_line'"
	fi
	alias top="/usr/bin/top -ocpu -R -s2"
fi

# Prompt {{{1
################################################################

if [ -n "$(which msmtp-queue 2>/dev/null)" -a -d ~/.msmtp.queue ] ; then
	export PROMPT_COMMAND="if [ -n \"\$(ls ~/.msmtp.queue)\" ] ; then echo $'\e[31mYou have messages waiting in outgoing mail queue.\e[0m' ; fi"
fi

function ps1_host {

	local host=$(hostname)

	case $host in
		schroeder|schroeder.*|schroeder-*)  color=99  ; host=schroeder ;;
		shermy|shermy-*)                    color=136 ; host=shermy ;;
		patty|patty.*|patty-*)              color=124 ; host=patty ;;
		marcie|marcie.*|marcie-*)           color=220 ; host=marcie ;;
		rat|rat.*|rat-*)                    color=249 ; host=rat ;;
		pig|pig.*|pig-*)                    color=212 ; host=pig ;;
		is*)                                color=160 ; host=$(hostname | sed 's/\.intra\.cea\.fr$//') ;;
		*)                                  color=255 ;;
	esac
	
	echo \\[$'\e[38;5;'$color'm'\\]$host\\[$'\e[0m'\\]
}

export PS1="\u@$(ps1_host):\W$ "

# Keyboard shortcuts {{{1
################################################################

# Define another key for beginning-of-line (originally C-a), so it doesn't conflict with `screen`. C-b was backward-char (i.e.: cursor left).
bind '"\C-b":beginning-of-line'

# Conda {{{1
################################################################################

conda_cfg=/etc/profile.d/conda.sh
if [[ -f $conda_cfg ]] ; then
	. $conda_cfg
fi
