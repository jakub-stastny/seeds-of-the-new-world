save-function-list

load ~/.zsh/environments/basic.zsh

alias emacs="emacs --load $PWD/.env/emacs.el"
alias et="bb -cp src -m jakub-stastny.et.runner"

alias build="./bin/convert.clj > output/content.en.tex && (cd output && context 'Seeds of the New World.tex')"

report-custom-functions
