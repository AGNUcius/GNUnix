# used for interactive or 'login' shells

export ANT_HOME=/usr/local/opt/ant
export MAVEN_HOME=/usr/local/opt/maven
export GRADLE_HOME=/usr/local/opt/gradle
export ANDROID_HOME=/usr/local/share/android-sdk
export ANDROID_NDK_HOME=/usr/local/share/android-ndk
export INTEL_HAXM_HOME=/usr/local/Caskroom/intel-haxm

export PATH=$ANT_HOME/bin:$PATH
export PATH=$MAVEN_HOME/bin:$PATH
export PATH=$GRADLE_HOME/bin:$PATH
export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH
export PATH=$ANDROID_HOME/build-tools/23.0.1:$PATH

export PATH="$HOME/bin:/usr/local/sbin:$PATH:"

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi


# fancy but sometimes slow git stuff:
# if [[ $(uname) == 'Darwin' ]]; then
#     which brew 2>&1 > /dev/null
#     if [[ $? != 0 ]]; then
#         pkg.SETUP #install brew
#         brew install git bash-completion
#     fi

#     bc=$(brew --prefix)/etc/bash_completion
#     if [ -f $bc ]; then
#         source $bc
#     fi
# fi

# which git > /dev/null
# if [[ $? == 0 ]]; then
#     GIT_PS1_SHOWDIRTYSTATE=true
#     GIT_PS1_SHOWCOLORHINTS=1
#     GIT_PS1_SHOWDIRTYSTATE=1

#     if [[ $(uname) == 'Darwin' ]]; then
#         gc=/Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash
#         gp=/Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-prompt.sh
#     else
#         gp=/usr/share/git/completion/git-prompt.sh
#         gp=/usr/share/git/completion/git-completion.bash
#     fi

#     [[ -f $gc ]] && source $gc
#     [[ -f $gp ]] && source $gp

#     PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }__git_ps1 '\u:\w' '\\\$ '"
#     # export PS1='\u $(__git_ps1 "(%s)")\$ '
#     # export PS1='[\u@mbp \w$(__git_ps1)]\$ '
# fi
