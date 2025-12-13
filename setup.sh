#!/usr/bin/env bash

echo "[INFO] Setting up $HOME"
for FILE in * .[^.]*; do
    [[ $FILE != .* ]] && continue
    [[ $FILE =~ ^(\.emacs\.d|\.git|\.config)$ ]] && continue
    echo "$FILE"
    ln -fn "$FILE" "$HOME/$FILE"
done

echo "[INFO] Setting up '.config' directory"
find .config/ -type f -print0 | while IFS= read -r -d $'\0' FILE; do
    echo "$FILE"
    ln -fn "$FILE" "$HOME/$FILE"
done
