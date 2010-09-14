#!/bin/bash

export FOGUS_HOME=`pwd`
export FOGUS_CONFIG=$FOGUS_HOME/config
export FOGUS_TOOLCHAIN=$FOGUS_HOME/toolchain
export FOGUS_LIB=$FOGUS_TOOLCHAIN/java/xlib
export TOOLCHAIN=$FOGUS_TOOLCHAIN

####
# Determine the OS type
####
os_type=`uname`

case $os_type in
    CYGWIN*)
        export FOGUS_OS=cygwin 
        ###
        # Cygwin is a special case.  First we set up its needed env
        # and then treat it like Windows afterwards
        ###
		###
		# Backup the old JAVA_HOME
		###
		export JH=$JAVA_HOME
        ;;
    Linux)
        export FOGUS_OS=linux    
        ;;
    Darwin)
        export FOGUS_OS=osx
        ;;
    *)
        echo "OS Type: $os_type is not supported"
        return
        ;;
esac


#####
# Setup common binaries
#####
export SCALA_HOME=$FOGUS_TOOLCHAIN/src/java/scala

#####
# Setup OS specific binaries
#####
source $FOGUS_CONFIG/osdep/$FOGUS_OS/$FOGUS_OS-environment.sh

export FOGUS_PATH=$TOOLCHAIN/bin

if [ "$FOGUS_PATH" != "${PATH:0:${#FOGUS_PATH}}" ]
then 
  PATH=$FOGUS_PATH:$PATH 
fi

export PATH

echo "-= Binaries available on this system =-"
echo "          ant:      "    `which ant | tail -n 1`
echo "          emacs:    "    `which emacs | tail -n 1`
echo "          dot:      "    `which dot | tail -n 1`
echo "          freemind: "    `which freemind | tail -n 1`
echo "          java:     "    `which java | tail -n 1`
echo "          javac:    "    `which javac | tail -n 1`
echo "          scala:    "    `which scala | tail -n 1`
echo "          lisp:     "    `which lisp | tail -n 1`

echo "-= Initializing the Minotaur Computing Environment =-"
echo "          FOGUS_HOME:      $FOGUS_HOME "
echo "          FOGUS_CONFIG:    $FOGUS_CONFIG "
echo "          FOGUS_TOOLCHAIN: $FOGUS_TOOLCHAIN "
echo "          FOGUS_OS:        $FOGUS_OS "
echo "--------------------------------------------------"
echo "PATH: $PATH"

