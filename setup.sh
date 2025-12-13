#!/usr/bin/env bash

echo "[INFO] Setting up $HOME"
for FILE in * .[^.]*; do
    if [[ ! $FILE =~ ^\. || $FILE =~ ^(\.emacs\.d|\.git|\.config)$ ]]; then
        continue
    fi
    echo "$FILE"
    ln -fn "$FILE" "$HOME/$FILE"
done

echo "[INFO] Setting up '.config' directory"
find .config/ -type f -print0 | while IFS= read -r -d $'\0' FILE; do
    echo "$FILE"
    ln -fn "$FILE" "$HOME/$FILE"
done

