function jj-workspace-open
    set workspace_directory (jj root)-workspace

    if test (count $argv) -eq 0
        set workspace_name (ls $workspace_directory | fzf --exit-0 --prompt="Select workspace: ")
    else
        set workspace_name $argv[1]
    end

    if test -z $workspace_name
        return 1
    end

    if not test -d $workspace_directory
        mkdir $workspace_directory
        echo "Create workspace directory '$workspace_directory'" >&2
    end

    set workspace_path $workspace_directory/$workspace_name

    if not jj workspace list | string match -q -r "^$workspace_name:"
        jj workspace add $workspace_path
    end

    cd $workspace_path
end
