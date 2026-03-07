#!/usr/bin/env bash

behind=$(git rev-list --count HEAD..origin/main 2>/dev/null)
if [ "$behind" -ne 0 ]; then
  echo "[WARN] The repository is $behind commits behind. Use 'git pull' to update."
  exit 1
fi

function ensure_parent_dir()
{
  local target="$HOME/$1"
  local parent_dir
  parent_dir=$(dirname "$target")
  if [ ! -d "$parent_dir" ]; then
    echo "[INFO] Creating directory '$parent_dir'"
    mkdir -p "$parent_dir"
  fi
}

function set_link_directory()
{
  echo "[INFO] Setting up '$1' directory"
  find $1 -type f -name ".gitignore" -prune -o -type f -print0 | while IFS= read -r -d $'\0' FILE; do
    echo "$FILE"
    ensure_parent_dir "$FILE"
    ln -sf "${PWD}/$FILE" "$HOME/$FILE"
  done
}

echo "[INFO] Setting up $HOME"
for FILE in * .[^.]*; do
  [[ $FILE != .* ]] && continue
  [[ $FILE =~ ^(\.emacs\.d|\.git|\.config|\.gitignore)$ ]] && continue
  echo "$FILE"
  ensure_parent_dir "$FILE"
  ln -sf "${PWD}/$FILE" "$HOME/$FILE"
done

set_link_directory ".config/"
set_link_directory ".emacs.d/"
