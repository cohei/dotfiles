if [ -f ~/.profile ]; then
    source ~/.profile
fi

if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

export LANG=ja_JP.UTF-8

export PS1='\n[\t] \W$(__git_ps1) $ '

# mainly for git commiting
if [ `uname` = 'Linux' ]; then
    export EDITOR='emacsclient --alternate-editor="" -c'
else
    export EDITOR='emacsclient --alternate-editor=""'
fi

function share_history {  # 以下の内容を関数として定義
    history -a  # .bash_historyに前回コマンドを1行追記
    history -c  # 端末ローカルの履歴を一旦消去
    history -r  # .bash_historyから履歴を読み込み直す
}
PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施

eval "$(rbenv init -)"

if [ $OSTYPE = linux-gnu ]; then
    if [ `psgrep uim-toolbar-qt4 | grep -v grep | wc -l` -eq 0 ]; then
        nohup uim-toolbar-qt4 -geometry +1500+0 &> /dev/null &
    fi
fi
