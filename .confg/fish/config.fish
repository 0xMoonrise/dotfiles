set fish_greeting ""
function fish_prompt
    set dir (basename $PWD)
    if test $PWD = $HOME
        set dir "~"
    end
	echo [(whoami)@(hostname) $dir]
	echo "\$ "
end
uv generate-shell-completion fish | source
