
# Source profile
[[ -f $HOME/.profile ]] && source $HOME/.profile

# Source bashrc
[[ $- == *i* && -f $HOME/.bashrc ]] && source $HOME/.bashrc
