#!/bin/sh

while IFS= read -r pkg || [ -n "$pkg" ]; do
    pipx install "$pkg"
done <.pipx.txt
