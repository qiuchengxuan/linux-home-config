[ -f ~/.antigen.zsh ] || curl https://cdn.rawgit.com/zsh-users/antigen/v1.4.1/bin/antigen.zsh > ~/.antigen.zsh
source ~/.antigen.zsh
source /usr/share/autojump/autojump.zsh
[ -f ~/.zsh_local ] && source ~/.zsh_local

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

alias ls='ls -G --color'
alias ll='ls -l -h -G --color'

alias find='noglob find'
alias locate='noglob locate'

setopt interactivecomments

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
if [[ $TERM == "xterm-256color" ]]; then
    bindkey "^[[H" beginning-of-line
    bindkey '^[[F' end-of-line
fi

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search

PROMPT='%(?..%K{red})%F{cyan}%n%F{blue}@%m:%F{yellow}%1~%f%(?..%k) '

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
