[ -f ~/.antigen.zsh ] || curl -L git.io/antigen > ~/.antigen.zsh
source ~/.antigen.zsh
if [ -f /usr/share/autojump/autojump.zsh ]; then
    source /usr/share/autojump/autojump.zsh
elif [ -f /usr/local/Cellar/autojump/22.5.1/share/autojump/autojump.zsh ]; then
    source /usr/local/Cellar/autojump/22.5.1/share/autojump/autojump.zsh
fi
[ -d ~/.fzf ] || git submodule update --init --recursive
[ -f ~/.fzf/bin/fzf ] || ~/.fzf/install
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen apply

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

export EDITOR=vim

alias ls='ls --color'
alias ll='ls -l -h --color'

alias sssh='TERM=xterm ssh'
alias tmux='tmux a || tmux'

setopt interactivecomments
setopt no_nomatch

WORDCHARS='*?_[]~=&;!#$%^(){}<>'

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "[3~" delete-char

PROMPT='%(?..%K{red})%F{cyan}%n%F{blue}@%m:%F{yellow}%1~%f%(?..%k) '

function precmd () {
    RPROMPT="%F{yellow}%U%~%f%u"
    git_root=$(git rev-parse --show-toplevel 2> /dev/null)
    [[ -z "$git_root" || "$HOME" == "$git_root" ]] && return

    branch=$(git branch -q | grep \* | cut -d ' ' -f 2)
    [ -z "$branch" ] && return
    git_status=$(git status -s --ignore-submodule | awk '{print $1}' | sort | uniq | tr -d '\n')
    git_prompt="%F{magenta}@git:${branch##* }%f"
    if [[ "$git_status" == *"M"* ]]; then
        git_prompt="%k%K{green}$git_prompt%k"
    elif [ ! -z "$git_status" ]; then
        git_prompt="%k%K{white}$git_prompt%k"
    fi
    RPROMPT="$RPROMPT$git_prompt"
}

if [ -f ~/.zsh_local ]; then
    source ~/.zsh_local
fi
