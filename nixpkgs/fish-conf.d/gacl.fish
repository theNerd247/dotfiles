function gacl -d "clone ssh general assembly github"
	set userName (echo $argv | cut -d '/' -f 1)
	set repo (echo $argv | cut -d '/' -f 2 | sed -e 's/.git//')

  git clone "git@git.generalassemb.ly:$argv" "$userName-$repo"
end
