#
#	Here we will list some useful git commands,
#	or bash functions which do some repository  operations;
#	


#--------------------------------------------------------------------
#	(1)
#	Lets consider problem:
#	you done some work in the project, 
#	commited your work to the main branch;
#	after that for next week you did nothing, but some other
#	users commited their work to the main branch.
#	Now you want now what did changed. 
#	There some ideas that will help in undarstanding the introduced 
#	changes in the project, which after short time of considering 
#	seems obious, but maybe at first glance doesn't come to mind:
#	- our last commit is good refering point 
#		git diff $OUR_LAST_COMMIT
#	- if the users were nice, each commit should group some changes;
#	- we want to see each commit's description since our last commit; 
#
#	Usage of below defined functions:
#	Define 
#	MY_LAST_COMMIT=<some commit>
#	e.g. MY_LAST_COMMIT=88f7ae8b9b9883451793c88f64d17620ff153e10#now run in bash#$ gdif-all#and 
#	run in terminal
#	$ gdif-all
#	and
#	$ gdif-one models
#	(first command shows all differences, second for one file, here
#	main/models/models.py)


function demo {
	#Just listing of some useful commands

	#
	#	HEAD - last commit
	#	HEAD~1 - before last commit
	#

	# To see only author, date and commit description
	git show --quiet HEAD~1

	# To have list of files which changed, or added
	git diff --name-only HEAD~1
	git diff --name-status HEAD~1
	git diff --stat --name-status --color HEAD~1	

	#To have list of files which were commited in a commit
	git show --name-status HEAD

	#Thats super nice thing;
	git show --stat --color HEAD~1
}


# a commit to which You want to refer the newest changes in the repo;
# export MY_LAST_COMMIT=88f7ae8b9b9883451793c88f64d17620ff153e10


function gdif-all {
	# @Description:
	# 	Shows shortly MY_LAST_COMMIT message and
	# 	-> which files were changed until current state
	# 	(if deleted/inserted/modified, how many lines were changed)
	# 	IMPORTANT: crucial command used here is:
	# 	git diff --stat --color $MY_LAST_COMMIT HEAD	
	#
	# @Args
	# 	Global variable MY_LAST_COMMIT must be set;
	#
	# @Usage
	# 	diff-see

	function space { echo ""; echo ""; }

	space	
	git show --quiet "$MY_LAST_COMMIT"
	space
	git diff --stat --color "$MY_LAST_COMMIT" HEAD
	space
	git diff --name-status "$MY_LAST_COMMIT" HEAD
	space
}
function gdif-all0 {
	# @Description
	# 	The most important part from previous function;
	# @Usage
	#	e.g. di-see_ HEAD~1
	#	e.g. di-see_ HEAD~2
	#	e.g. di-see_ HEAD~3

	git diff --stat --color "$1" HEAD
}

function gdif-one {
	# @Description
	#	Shows what was changed in ONE PARTICULAR file
	#	since our last commit
	# @Usage
	#	You dont put the whole path to a file,
	#	just name without extension; Works only if there
	#	exists one file with that name in the project;
	#	e.g. dif-forOneFile models
	#		(show diff for src/models/models.py)

	currPos=$(pwd); cd "$projectRoot__" ;
	git diff "$MY_LAST_COMMIT" -- $(find . -name "$1".py) 
	cd "$currPos" 
}
function gdif-one0 {
	# @Description the same as above, but you can choose commit
	# @Args
	#	- name of file
	#	- commit
	#	- global variable currPos must be defined 
	#		pointing to root of project;
	# @Usage e.g. 
	#	dif-forOneFile_ models HEAD~1

	currPos=$(pwd); cd "$projectRoot__" ;
	git diff "$MY_LAST_COMMIT" -- $(find . -name "$1".py) 
	cd "$currPos" 
}
#--------------------------------------------------------------------