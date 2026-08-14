# Exit early if the shell is not running interactively (e.g., in scripts or SSH commands)
[[ $- != *i* ]] && return


# Helper function to prepend a directory to PATH if it isn't already included
append_path () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="$1:${PATH:+$PATH:}"
            ;;
    esac
}

# Prepend user's local bin directory to system PATH
append_path "$HOME/.local/bin"

# Default system text editors (VISUAL for full-screen editors, EDITOR for quick edits)
export VISUAL=less
export EDITOR=vim


# Prevent duplicate consecutive entries and lines starting with space from being saved
HISTCONTROL=ignoreboth

# Append new history commands to the file instead of overwriting on exit
shopt -s histappend

# Maximum number of history lines stored in memory
HISTSIZE=1000

# Maximum number of history lines stored in the history file (~/.bash_history)
HISTFILESIZE=2000


# Update terminal window dimensions (LINES and COLUMNS) after every command execution
shopt -s checkwinsize

# Restore cursor blinking state on every prompt refresh
# PROMPT_COMMAND+=('echo -e -n "\\x1b[0 q"')

# Colored prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# PS1='\u@\h:\w\$ '

# Dynamically set terminal window title to user@host:dir for xterm/rxvt windows
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac


# Enable colored output for file listings and grep commands if dircolors is available
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Highlight GCC compiler errors, warnings, and notes with color output
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Enable advanced command-line auto-completion if non-POSIX mode and scripts exist
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias ln='ln -i'
alias vi='nvim'
alias vim='nvim'
alias poweroff='systemctl poweroff'
alias reboot='systemctl reboot'
alias remove-orphans='pacman -Qdtq >/dev/null && sudo pacman -Rns $(pacman -Qdtq) || echo "Nothing to do.."'
alias remote-bk='borg-create bx11 borg-repository'
alias remote-ex='borg-extract bx11 borg-repository'
alias local-bk='borg-create lcserv raid/backup'
alias local-ex='borg-extract lcserv raid/backup'

set -o vi

if [ -f "$HOME/.bash_aliases" ]; then
    . "$HOME/.bash_aliases"
fi

