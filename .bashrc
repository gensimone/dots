# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Messing with environment variables.
append_path () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="$1:${PATH:+$PATH:}"
    esac
}
append_path $HOME/.local/bin
export VISUAL=less
export EDITOR=vi

# Prompt settings.
PS1='[\u@\h \W]\$ '

# Restore cursor blink settings.
PROMPT_COMMAND+=('echo -e -n "\\x1b[0 q"')

# Enable vi-mode.
set -o vi

# Some aliases.
alias c='clear'
alias df='df -h'
alias du='du -h'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias ln='ln -i'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias ls='ls --color=auto'
alias pe='eval $(poetry env activate)'
alias py='python3'
alias python='python3'
alias vi='nvim'
alias vim='nvim'

# Source extra aliases.
if [ -f ~/.bash_aliases ]; then
	. ~/.bash_aliases
fi
