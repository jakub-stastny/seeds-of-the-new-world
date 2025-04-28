save-function-list

load ~/.zsh/environments/basic.zsh

alias emacs="emacs --load $PWD/.env/emacs.el"
alias et="bb -cp src -m jakub-stastny.et.runner"

alias build.en="./bin/convert.clj > output/content.en.tex && (cd output && context 'Seeds of the New World.tex')"
alias build.es="./bin/convert.clj > output/content.es.tex && (cd output && context 'Semillas del Nuevo Mundo.tex')"
alias build.cz="./bin/convert.clj > output/content.cz.tex && (cd output && context 'Seminka Noveho Sveta.tex')"

report-custom-functions
