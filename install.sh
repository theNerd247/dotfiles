#!/bin/bash

#go to our install directory
BASEPATH="$1"

if [[ -z $BASEPATH ]]; then
	BASEPATH="$HOME"
fi

if [[ ! -e "$BASEPATH/.dotfiles" ]]; then
	echo "$BASEPATH/.dotfiles doesn't exist!"
	exit 1
fi

echo "installing system to: $BASEPATH"

function getInstPath()
{
	echo "$BASEPATH/.$(basename $1)"
}

function lnk()
{
	if [[ -z $2 ]]; then
		echo "couln't get install path"
		exit 1
	fi

	instPath="$(getInstPath $2)"

	echo "linking $BASEPATH/.dotfiles/$1 to $instPath"

	rm -ri $instPath

	ln -s -i "$BASEPATH/.dotfiles/$1" "$instPath"
}

function inst()
{
	instPath="$(getInstPath $1)"

	echo "running install at $instPath"
	cd "$instPath"
	./install.sh
}


#create the symbolic links to the config paths

#lnk astylerc
lnk fonts fonts
#lnk oh-my-zsh oh-my-zsh
#lnk vimrc vim
#lnk "vimrc/vimrc" vimrc
#lnk xmobar/xmobarcc
#lnk xmonad
#lnk Xresources
#lnk zshrc zshrc
#lnk xinitrc

#inst vim
#inst oh-my-zsh
inst fonts
