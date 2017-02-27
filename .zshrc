source /usr/share/autojump/autojump.zsh
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
plugins=(zsh-syntax-highlighting)

HISTFILE=$HOME/.zsh_history                                                                                             
HISTSIZE=10000                                                                                                          
SAVEHIST=10000

export PATH=~/.cargo/bin:$PATH
export RUST_SRC_PATH=~/dev/rust/src
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/

alias ls='ls -G --color'
alias ll='ls -l -G --color'

setopt interactivecomments

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

function precmd () {
    branch=`git branch 2> /dev/null | grep \* | cut -d ' ' -f 2`
    if [ -n "$branch" ]; then
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
