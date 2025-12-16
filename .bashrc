#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

append_path () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}

append_path $HOME/.local/bin
append_path $HOME/.cargo/bin

PS1='[\W]\$ '

set -o vi

lmlbk() {
        lmount $@ && locbk $HOME/external && lumount
}

h() {
        "$@" --help | less
}

export EDITOR=vi
export VISUAL=vi

# https://github.com/python/cpython/issues/118840
export PYTHON_BASIC_REPL=1

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

eval "$(starship init bash)"
