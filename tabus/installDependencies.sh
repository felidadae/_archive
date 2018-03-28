# Here we can put commands to install some needed packages;
# we should create virtualenv probably;

OS_KERNEL__=$(uname -s 2>/dev/null | tr "[:upper:]" "[:lower:]")



if [ "$OS_KERNEL__" == "darwin" ]; 
then
	#MAC OS X
	;
fi

if [ "$OS_KERNEL__" == "linux" ]; 
then
	#LINUX
	OS__=$(lsb_release -si)
	OS_VER__=$(lsb_release -sr)

	if [ "$OS__" == "Ubuntu" ] && [ "$OS_VER__" == "16.04"  ]; 
	then
		;
	fi
fi