#
#	@Description
#		Here we put some useful bash functions
#	@Important note:
#		currPos=$(pwd); cd "$projectRoot__";
#			is to remember current position (path)
# 			and move to root dir of project;
#		cd "$currPos"
#			is to return to previous path;
#		Thanks to that we can run this commands from
#		any position;  

function findRoot {
	currPos=$(pwd)
	while [[ $(ls -1a | grep ".git$") == "" ]]; do cd ..; done
	export projectRoot__=$(pwd)
	cd "$currPos" 
}
findRoot

function pp-reload {
	RELOADED=1
	currPos2=$(pwd); cd "$projectRoot__"/.dev; 
	for ifile in *; do
		[ -e "$ifile" ] || continue
		[[ "$ifile" == *"tools"* ]] || continue
		[[ "$RELOADED" == "" ]] && [[ "$ifile" == "tools.sh" ]] \
			&& continue
		# echo "SOURCE  M${ifile}M"
		source "$ifile"
	done
	cd "$currPos2"
	RELOADED=1
}
if [[ "$RELOADED" == "" ]]; then
	pp-reload
fi

function pp-shell {
	currPos=$(pwd); cd "$projectRoot__"; 
	python3 manage.py shell
	cd "$currPos"
}

function pp-migrate {
	currPos=$(pwd); cd "$projectRoot__"; 
	python3 manage.py makemigrations main
	python3 manage.py migrate
	cd "$currPos"
}

function pp-cleanMigrationsAndDB {
	currPos=$(pwd); cd "$projectRoot__"; 
	rm -r main/migrations/*
	rm db.sqlite3
	cd "$currPos"
}

function pp-testsrun {
	currPos=$(pwd); cd "$projectRoot__"; 
	python3 -Wall manage.py test --pattern *__test.py -v 3
	cd "$currPos"
}

function pp-listPyFiles-flat {
	currPos=$(pwd);cd "$projectRoot__";
	find .  -name '*.py'
	cd "$currPos" 	
}

function pp-listPyFiles {
	currPos=$(pwd); cd "$projectRoot__";
	tree -P "*.py" \
		-I '*__pycache__*|*__init__.py|migrations'
	cd "$currPos" 	
}
alias ppl="pp-listPyFiles"
alias pl="pptt"

function pp-vim {
	currPos=$(pwd); cd "$projectRoot__" ;
	vim $(find . -name "$1".py) 
	cd "$currPos" 
}
alias pv="pp_vim"

function pp-listPyFiles-main {
	currPos=$(pwd); cd "$projectRoot__";
	tree -P "*.py" \
		-I '*__pycache__*|*__init__.py|migrations'

	cd "$currPos" 
}


#
#	finishing
#
function pp-listfun {
	currPos=$(pwd); cd "$projectRoot__"/.dev; 
	for ifile in *; do
		[ -e "$ifile" ] || continue
		[[ "$ifile" == *"tools"* ]] || continue
		perl -nle 'print "$1" if /^\s*function (.*)\s*\{/' $ifile
	done
	cd "$currPos"
}
pp-listfun