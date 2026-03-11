#
# ~/.bash_profile
#

append_path () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="$1:${PATH:+$PATH:}"
    esac
}

append_path $HOME/.local/bin

[[ -f ~/.bashrc ]] && . ~/.bashrc
