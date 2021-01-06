if not functions --query fundle
    eval (curl --silent --fail --location https://git.io/fundle-install)
end

fundle plugin 'jethrokuan/z'
fundle init
