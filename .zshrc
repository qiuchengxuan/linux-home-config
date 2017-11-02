[ -f ~/.antigen.zsh ] || curl https://cdn.rawgit.com/zsh-users/antigen/v1.4.1/bin/antigen.zsh > ~/.antigen.zsh
source ~/.antigen.zsh
if [ -f /usr/share/autojump/autojump.zsh ]; then
    source /usr/share/autojump/autojump.zsh
elif [ -f /usr/local/Cellar/autojump/22.5.1/share/autojump/autojump.zsh ]; then
    source /usr/local/Cellar/autojump/22.5.1/share/autojump/autojump.zsh
fi
[ -d ~/.fzf ] || git submodule update --init --recursive
[ -f ~/.fzf/bin/fzf ] || ~/.fzf/install
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.zsh_local ] && source ~/.zsh_local

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

export EDITOR=vim

alias ls='ls --color'
alias ll='ls -l -h --color'

alias find='noglob find'
alias locate='noglob locate'
alias java='noglob java'
alias yum='noglob yum'
alias scp='noglob scp'

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
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

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
