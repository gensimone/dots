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

PS1='[\W]$ '

set -o vi

export EDITOR=vi
export VISUAL=vi
export PROMPT_COMMAND="echo"
export PYTHON_BASIC_REPL=1

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

lmlbk() {
        lmount $@ && locbk && lumount
}

h() {
        "$@" --help | less
}
