function gclean -d "cleans remote tracking branches"
  git branch --merged master | grep -v master | xargs -L 1 git branch -d
end
