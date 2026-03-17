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

export QT_QPA_PLATFORM=wayland
export QT_QPA_PLATFORMTHEME=qt6ct

[[ -f ~/.bashrc ]] && . ~/.bashrc
