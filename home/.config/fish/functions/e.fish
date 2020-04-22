if test (uname) = "Darwin"
    alias e 'emacsclient --tty --no-wait --alternate-editor="open -a emacs"'
else
    alias e 'emacsclient --alternate-editor="" --create-frame'
end
