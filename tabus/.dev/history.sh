projectName="tabus"
appName="main"

django-admin startproject "$projectName"
cd "$projectName"

mkdir .dev

# Create sublime-project
echo '{' >> .dev/"$projectName.sublime-project"
echo '	"folders":' >> .dev/"$projectName.sublime-project"
echo '	[' >> .dev/"$projectName.sublime-project"
echo '		{' >> .dev/"$projectName.sublime-project"
echo '			"path": "../"' >> .dev/"$projectName.sublime-project"
echo '		}' >> .dev/"$projectName.sublime-project"
echo '	]' >> .dev/"$projectName.sublime-project"
echo '}' >> .dev/"$projectName.sublime-project"

touch .dev/tools.sh
touch .dev/.tmp.sh

# Create .gitignore
touch .gitignore
echo "#.gitignore" >> .gitignore
echo "" >> .gitignore
echo "#development" >> .gitignore
echo ".dev/*.sublime-workspace" >> .gitignore
echo "" >> .gitignore
echo "#python temporary" >> .gitignore
echo "*.pyc" >> .gitignore
echo "*.pyo" >> .gitignore
echo "" >> .gitignore
echo "#db" >> .gitignore
echo "db.sqlite3" >> .gitignore

python manage.py startapp main
