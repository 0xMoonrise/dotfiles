#!/usr/bin/env bash

echo "Setting up $HOME"
for FILE in * .[^.]*; do
    if [[ ! $FILE =~ ^\. || $FILE =~ ^(\.emacs\.d|\.git)$ ]]; then
        continue
    fi
    echo "$FILE"
    ln -fn "$FILE" "$HOME/$FILE"
done

echo "Setting up '.config' directory"
find .config/ -type f -print0 | while IFS= read -r -d $'\0' FILE; do
    echo "$FILE"
    ln -fn "$FILE" "$HOME/$FILE"
done

