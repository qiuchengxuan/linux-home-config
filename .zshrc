[ -f .antigen.zsh ] || curl https://cdn.rawgit.com/zsh-users/antigen/v1.4.1/bin/antigen.zsh > .antigen.zsh
source .antigen.zsh
source /usr/share/autojump/autojump.zsh
[ -f .zsh_local ] && source .zsh_local

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

alias ls='ls -G --color'
alias ll='ls -l -G --color'

alias find='noglob find'

setopt interactivecomments

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

function precmd () {
    RPROMPT="%F{yellow}%U%~%f%u"
    git_root=$(git rev-parse --show-toplevel 2> /dev/null)
    [[ -z "$git_root" || "$HOME" == "$git_root" ]] && return

    branch=$(git branch -q | grep \* | cut -d ' ' -f 2)
    [ -z "$branch" ] && return
    git_status=$(git status -s --ignore-submodule)
    if [ -z "$git_status" ]; then
        RPROMPT="$RPROMPT%F{magenta}@git:${branch##* }%f"
    else
        RPROMPT="$RPROMPT%f%K{red}@git:${branch##* }%k"
    fi
}

if [ -n "$SSH_CLIENT" ]; then
    PROMPT='%F{yellow}%n%F{cyan}@%m:%(?.%f.%F{red})%1~%f '
else
    PROMPT='%F{yellow}%n%F{cyan}@%(?.%f.%F{red})%1~%f '
fi
