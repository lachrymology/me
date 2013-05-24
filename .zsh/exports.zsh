export CDPATH=.:~/src:~/src/personal:~/src/opensource:~/src/fogus:~/src/configs:~/Documents:~/src/security

#export JAVA_HOME=$(/usr/libexec/java_home)
#export JDK_HOME=$(/usr/libexec/java_home)
export CLOJURE_HOME=~/src/opensource/clojure
export CLOJURESCRIPT_HOME=~/src/opensource/clojure/clojurescript

export PATH=/usr/local/bin:~/src/opensource/etc/scripts:~/eclipse:~/src/opensource/android-sdk-linux_x86/tools:$CLOJURESCRIPT_HOME/bin:~/src/opensource/cljs-watch:$PATH

# Setup terminal, and turn on colors
export TERM=xterm-256color
export LSCOLORS=gxfxcxdxbxegedabagacad
export CLICOLOR=1

# Enable color in grep
export GREP_OPTIONS='--color=auto' 
export GREP_COLOR='3;33'

export PAGER=most
export EDITOR="emacs"
