function fish_prompt --description 'Write out the prompt'
	
    if not set -q __git_cb
	    set __git_cb (set_color yellow)""(git branch ^/dev/null | grep \* | sed 's/* //')(set_color normal)""
    end

    switch "$USER"

        case root toor
            printf '%s%s %s%s%s# ' (set_color -o red) $USER (set_color -o blue) (prompt_pwd) (set_color -o normal)

        case '*'
            printf '\f\r%s%s %s\f\r %s$%s ' (set_color -o blue) "$PWD" "$__git_cb" (set_color -o green) (set_color -o normal)
    end
end
