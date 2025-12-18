#!/usr/bin/env bash

function set_link_directory()
{
  echo "[INFO] Setting up '$1' directory"
  find $1 -type f -print0 | while IFS= read -r -d $'\0' FILE; do
    echo "$FILE"
    ln -fn "$FILE" "$HOME/$FILE"
  done  
}

echo "[INFO] Setting up $HOME"
for FILE in * .[^.]*; do
  [[ $FILE != .* ]] && continue
  [[ $FILE =~ ^(\.emacs\.d|\.git|\.config)$ ]] && continue
  echo "$FILE"
  ln -fn "$FILE" "$HOME/$FILE"
done

set_link_directory ".config/"

set_link_directory ".emacs.d/"
