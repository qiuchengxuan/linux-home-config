[ -f .antigen.zsh ] || curl https://cdn.rawgit.com/zsh-users/antigen/v1.4.1/bin/antigen.zsh > .antigen.zsh
source .antigen.zsh
source /usr/share/autojump/autojump.zsh
[ -f .zsh_local ] && source .zsh_local

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

alias ls='ls -G --color'
alias ll='ls -l -G --color'

setopt interactivecomments

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

function precmd () {
    branch=`git branch 2> /dev/null | grep \* | cut -d ' ' -f 2`
    if [[ -n "$branch" && "$(pwd)" != "$HOME" ]]; then
        RPROMPT="%F{yellow}%U%~%f%u%F{magenta}@git:${branch##* }%f"
    else
        RPROMPT="%F{yellow}%U%~%f%u"
    fi
}

if [ -n "$SSH_CLIENT" ]; then
    PROMPT='%F{yellow}%n%F{cyan}@%m:%(?.%F{green}.%F{red})%1~%f '
else
    PROMPT='%F{yellow}%n@%(?.%F{green}.%F{red})%1~%f '
fi
