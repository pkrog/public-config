# vi: fdm=marker

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

# Alias {{{1
################################################################

alias lock='/System/Library/CoreServices/"Menu Extras"/User.menu/Contents/Resources/CGSession -suspend'
#alias vscreen="cd $HOME/dev && screen vim private-notes/todo.md && cd -"
#alias vtmux="cd $HOME/dev && tmux new-session vim private-notes/todo.md && cd -"
alias xterm="xterm -bg black -cr white -fg white"
alias ls="ls -Fh"
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

# Top {{{2
if [ "$PLATFORM" = "darwin" ] ; then
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

# Terminal color {{{1
################################################################

if [[ $COLORTERM == xfce4-terminal || $TERM == xterm-256color-italic ]] ; then
	TERM=xterm-256color
fi

# Keyboard shortcuts {{{1
################################################################

# Define another key for beginning-of-line (originally C-a), so it doesn't conflict with `screen`. C-b was backward-char (i.e.: cursor left).
bind '"\C-b":beginning-of-line'
