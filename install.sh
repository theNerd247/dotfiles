#!/bin/bash

#go to our install directory
BASEPATH="$1"

if [[ -z $BASEPATH ]]; then
	BASEPATH="$HOME"
fi

echo "BASEPATH: $BASEPATH"

function getInstPath()
{
	echo ".$(basename $1)"
}

function lnk()
{
	cd $BASEPATH

	if [[ -z $2 ]]; then
		instPath="$(getInstPath $1)"
	else
		instPath="$(getInstPath $2)"
	fi

	ln -s -i "$HOME/.dotfiles/$1" "$instPath"
}

function inst()
{
	cd "$BASEPATH/$(getInstPath $1)"
	./install.sh
}


#create the symbolic links to the config paths

#lnk astylerc
lnk fonts
lnk oh-my-zsh 
lnk vimrc vim
lnk vimrc/vimrc 
#lnk xmobar/xmobarcc
#lnk xmonad
#lnk Xresources
lnk zshrc
#lnk xinitrc

inst vim
inst oh-my-zsh
